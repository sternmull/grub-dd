/* dd.c - copy file to disk  */
/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 2003,2005,2007,2008  Free Software Foundation, Inc.
 *
 *  GRUB is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  GRUB is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GRUB.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <grub/dl.h>
#include <grub/file.h>
#include <grub/disk.h>
#include <grub/term.h>
#include <grub/misc.h>
#include <grub/extcmd.h>
#include <grub/i18n.h>

GRUB_MOD_LICENSE ("GPLv3+");


#define IMAGE_MAGIC "\0ntfsclone-image"
#define IMAGE_MAGIC_SIZE 16
#define NTFS_MAX_CLUSTER_SIZE	65536

struct image_hdr_raw {
	char magic[IMAGE_MAGIC_SIZE];
	grub_uint8_t major_ver;
	grub_uint8_t minor_ver;
	grub_unaligned_uint32_t cluster_size;
	grub_unaligned_uint64_t device_size;
	grub_unaligned_uint64_t nr_clusters;
	grub_unaligned_uint64_t inuse;
} GRUB_PACKED;

struct image_hdr {
   grub_uint32_t cluster_size;
   grub_uint64_t device_size;
   grub_uint64_t nr_clusters;
   grub_uint64_t inuse;
};

static void print_progress(grub_uint64_t cur, grub_uint64_t total)
{
   static int lastProgress = -1;
   if (!total)
      return;

   /* Division of uint64 is not available... */
   int curProgress = (grub_uint32_t)(cur * 100) / (grub_uint32_t)total;
   if (curProgress != lastProgress)
   {
      lastProgress = curProgress;
      grub_printf("\r%d%%", curProgress);
      grub_refresh();
   }
}

static const struct grub_arg_option options[] =
  {
    {"ntfs", -1, 0, N_("Interpret the input as image from ntfsclone instead."), 0, 0},
    {0, 0, 0, 0, 0, 0}
  };


static grub_err_t
grub_cmd_dd (grub_extcmd_context_t ctxt, int argc, char **args)
{
  int ntfs = ctxt->state[0].set;
  grub_disk_t disk;
  grub_file_t file;
  grub_uint8_t * buf = 0;
  grub_off_t srcSize;
  grub_off_t dstSize;
  grub_err_t err = 0;

  if (argc < 1)
    return grub_error (GRUB_ERR_BAD_ARGUMENT, "filename required");
  if (argc < 2)
    return grub_error (GRUB_ERR_BAD_ARGUMENT, "device name required");

  file = grub_file_open (args[0]);
  if (! file)
    return grub_errno;

  if (args[1][0] == '(' && args[1][grub_strlen (args[1]) - 1] == ')')
    {
      args[1][grub_strlen (args[1]) - 1] = 0;
      disk = grub_disk_open (args[1] + 1);
      args[1][grub_strlen (args[1])] = ')';
    }
  else
    disk = grub_disk_open (args[1]);

  if (! disk)
    {
      grub_file_close(file);
      return grub_errno;
    }

  dstSize = grub_disk_get_size (disk) << GRUB_DISK_SECTOR_BITS;

  for (;;)
  {
     if (ntfs)
     {
        struct image_hdr_raw rawHdr;
        struct image_hdr hdr;
        grub_uint32_t dataOffset;
        grub_uint32_t toSkip;
        grub_uint64_t clusterSectors;
        grub_uint64_t pos;
        grub_uint64_t writtenClusters;

        if (grub_file_read(file, &rawHdr, sizeof(rawHdr)) != sizeof(rawHdr))
        {
           err = grub_error (GRUB_ERR_FILE_READ_ERROR, "reading header of ntfsclone image");
           break;
        }

        if (grub_memcmp(rawHdr.magic, IMAGE_MAGIC, IMAGE_MAGIC_SIZE))
        {
           err = grub_error (GRUB_ERR_BAD_FILE_TYPE, "file is not an ntfsclone image");
           break;
        }

        if (rawHdr.major_ver != 10)
        {
           err = grub_error (GRUB_ERR_BAD_FILE_TYPE, "image is from an unsupported version of ntfsclone");
           break;
        }

        hdr.cluster_size = grub_le_to_cpu32(grub_get_unaligned32(&rawHdr.cluster_size));
        hdr.device_size = grub_le_to_cpu64(grub_get_unaligned64(&rawHdr.device_size));
        hdr.nr_clusters = grub_le_to_cpu64(grub_get_unaligned64(&rawHdr.nr_clusters));
        hdr.inuse = grub_le_to_cpu64(grub_get_unaligned64(&rawHdr.inuse));

        grub_printf("image details:\n");
        grub_printf("  cluster_size:  %u\n", hdr.cluster_size);
        grub_printf("  device_size:   %llu\n", hdr.device_size);
        grub_printf("  clusters:      %llu\n", hdr.nr_clusters);
        grub_printf("  used clusters: %llu\n", hdr.inuse);

        if (hdr.cluster_size % GRUB_DISK_SECTOR_SIZE)
        {
           err = grub_error (GRUB_ERR_BAD_DEVICE, "image cluster size is not a multiple of grub sector size");
           break;
        }

        clusterSectors = hdr.cluster_size / GRUB_DISK_SECTOR_SIZE;

        if (dstSize != GRUB_DISK_SIZE_UNKNOWN && hdr.cluster_size * hdr.nr_clusters > dstSize)
        {
           err = grub_error (GRUB_ERR_OUT_OF_RANGE, "image is bigger than destination device");
           break;
        }

        if (grub_file_read(file, &dataOffset, 4) != 4)
        {
           err = grub_error (GRUB_ERR_FILE_READ_ERROR, "failed to read data offset");
           break;
        }

        dataOffset = grub_le_to_cpu32(dataOffset);

        if (dataOffset < grub_file_tell(file))
        {
           err = grub_error (GRUB_ERR_FILE_READ_ERROR, "invalid data offset");
           break;
        }

        toSkip = dataOffset - grub_file_tell(file);
        while (toSkip)
        {
           /* NOTE: As the file may be unable to seek the bytes are skipped by reading them */
           grub_uint8_t trash[32];
           grub_uint32_t n = toSkip;
           if (n > sizeof(trash))
              n = sizeof(trash);
           if (grub_file_read(file, trash, n) != (grub_ssize_t)n)
           {
              err = grub_error (GRUB_ERR_FILE_READ_ERROR, "failed to skip to data offset");
              break;
           }
           toSkip -= n;
        }

        if (err)
           break;

        buf = grub_malloc(hdr.cluster_size);

        if (!buf)
        {
           err = grub_error (GRUB_ERR_OUT_OF_MEMORY, "failed to allocate buffer for the cluster");
           break;
        }

        pos = 0;
        writtenClusters = 0;
        while (pos < hdr.nr_clusters && !err)
        {
           grub_uint8_t cmd;
           grub_uint64_t gapClusters;
           if (grub_file_read(file, &cmd, 1) != 1)
           {
              err = grub_error (GRUB_ERR_FILE_READ_ERROR, "failed to read next command from image");
              break;
           }

           switch (cmd)
           {
           case 0: /* gap */
              if (grub_file_read(file, &gapClusters, sizeof(gapClusters)) != sizeof(gapClusters))
              {
                 err = grub_error (GRUB_ERR_FILE_READ_ERROR, "failed to read gap size from image");
                 break;
              }
              gapClusters = grub_le_to_cpu64(gapClusters);
              if (hdr.nr_clusters - pos < gapClusters)
                 err = grub_error (GRUB_ERR_OUT_OF_RANGE, "image declares gap that extends beyond its size");

              pos += gapClusters;
              break;
           case 1: /* cluster */
              if (grub_file_read(file, buf, hdr.cluster_size) != (grub_ssize_t)hdr.cluster_size)
              {
                 err = grub_error (GRUB_ERR_FILE_READ_ERROR, "failed to read cluster");
                 break;
              }
              if (grub_disk_write(disk, pos * clusterSectors, 0, hdr.cluster_size, buf))
              {
                 err = grub_error (GRUB_ERR_WRITE_ERROR, "failed to write cluster");
                 break;
              }
              ++pos;
              ++writtenClusters;
              break;
           default:
              err = grub_error(GRUB_ERR_FILE_READ_ERROR, "image contains invalid command code");
           }

           print_progress(writtenClusters, hdr.inuse);
        }
     }
     else
     {
        grub_uint32_t bufSectors = 128;
        grub_ssize_t bufSize = GRUB_DISK_SECTOR_SIZE * bufSectors;
        grub_disk_addr_t sector = 0;
        srcSize = grub_file_size (file);

        if (srcSize != GRUB_FILE_SIZE_UNKNOWN && dstSize != GRUB_DISK_SIZE_UNKNOWN && srcSize > dstSize)
        {
           err = grub_error (GRUB_ERR_BAD_ARGUMENT, "source file bigger than capacity of destination");
           break;
        }

        buf = grub_malloc(bufSize);
        if (!buf)
        {
           err = grub_error(GRUB_ERR_OUT_OF_MEMORY, "failed to allocate buffer");
           break;
        }

        for (;;)
          {
            grub_ssize_t size = grub_file_read (file, buf, bufSize);
            if (size < 0)
            {
               err = grub_error(GRUB_ERR_FILE_READ_ERROR, "failed to read from source");
               break;
            }

            if (!size)
               break;

            if (dstSize != GRUB_DISK_SIZE_UNKNOWN && (sector << GRUB_DISK_SECTOR_BITS) + size > dstSize)
            {
               err = grub_error(GRUB_ERR_OUT_OF_RANGE, "input exceeds capacity of destination");
               break;
            }

            if (grub_disk_write (disk, sector, 0, size, buf))
            {
               err = grub_errno;
               break;
            }

            if (size < bufSize)
               break;

            sector += bufSectors;

            if (srcSize != GRUB_FILE_SIZE_UNKNOWN)
               print_progress(sector, srcSize >> GRUB_DISK_SECTOR_BITS);
            else
            {
               static grub_disk_addr_t lastMiB = -1;
               grub_disk_addr_t mib = (sector << GRUB_DISK_SECTOR_BITS) >> 20;
               if (mib != lastMiB)
               {
                  lastMiB = mib;
                  grub_printf("\r%lluMiB written", mib);
               }
            }
          }
     }
     break;
  }

  grub_disk_close (disk);
  grub_file_close (file);
  grub_free(buf);

  grub_printf(err ?
                 "\rFAILED!                                \n":
                 "\rdone.                                  \n");

  return err;
}

static grub_extcmd_t cmd;

GRUB_MOD_INIT(dd)
{
  cmd = grub_register_extcmd ("dd", grub_cmd_dd, 0,
      N_("FILE DEVICE"),
      N_("Copies data from the source file to the device. "
      "Use --ntfs to extract an image that was created with ntfsclone."),
      options);
}

GRUB_MOD_FINI(dd)
{
  grub_unregister_extcmd (cmd);
}
