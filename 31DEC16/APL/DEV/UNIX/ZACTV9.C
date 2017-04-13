#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#include <unistd.h>
                                       /* If <unistd.h> can not be   */
                                       /* found you do not have a    */
                                       /* POSIX.1 compliant system   */
                                       /* and ZACTV9 requires local  */
                                       /* development.               */
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <signal.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

                                       /* We use our own prototypes   */
                                       /* for string functions if we  */
                                       /* are not using an ANSI C     */
                                       /* compiler so that we don't   */
                                       /* have to worry about the use */
                                       /* of <strings.h> on older BSD */
                                       /* systems.                    */
#ifdef __STDC__
#include <string.h>
#else
extern int strcmp();
extern char *strcpy();
extern int strlen();
#endif

                                       /* The malloc prototype may    */
                                       /* also be stored in           */
                                       /* unpredictable headers if    */
                                       /* not using ANSI C.           */
#ifdef __STDC__
#include <stdlib.h>
#else
extern char *malloc();
#endif
#include <AIPSPROTO.H>

#define PMODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define CMODE (O_WRONLY | O_CREAT | O_TRUNC | O_EXCL)

extern int msg_fd;                    /* Message terminal file        */
                                      /* descriptor (shared by all    */
                                      /* AIPS processes in this       */
                                      /* session) if messages are not */
                                      /* directed to the controlling  */
                                      /* terminal (-1 if they are)    */

#if __STDC__
   void zactv9_(int *flen, char fulnam[], int *plen, char cpname[],
      int *idbg, pid_t *pid, int *syserr, int *ierr)
#else
   void zactv9_(flen, fulnam, plen, cpname, idbg, pid, syserr, ierr)
   int *flen, *plen, *idbg, *pid, *syserr, *ierr;
   char fulnam[], cpname[];
#endif
/*--------------------------------------------------------------------*/
/*! starts requested load module under requested process name         */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2003                                   */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/*  ZACTV9 will execute the load module "fulnam" as a process/sub-    */
/*  process called "cpname" which consists of the program name with a */
/*  POPS number suffix x (e.g., AIPSx, BATERx, AIPSCx, AIPSBx, QMNGRx */
/*  or <taskname>x).  AIPSx an BATERx are initiated as processes that */
/*  replace the process that invokes ZACTV9 (i.e., without first      */
/*  forking a subprocess).  This means that in the case of AIPSx and  */
/*  BATERx, there is no return from ZACTV9.  The startup programs     */
/*  ZSTRTA and ZSTRTB invoke ZACTV9 (via ZACTV8) as part of the       */
/*  process of initiating an interative AIPS or BATER session.        */
/*  ZACTV9 is only called by ZACTV8. Note that the AIPS daemon        */
/*  DAIPx is treated exactly as AIPSx.                                */
/*                                                                    */
/*  Inputs:                                                           */
/*     FLEN     I          Length in characters of "fulnam"           */
/*     FULNAM   C*(flen)   Full path name of the required executable  */
/*                         module                                     */
/*     PLEN     I          Length in characters of "cpname"           */
/*     CPNAME   C*(plen)   Desired process name (6 characters or      */
/*                         less = program name + POPS number)         */
/*     IDBG     I          1 => start in DEBUGger if possible         */
/*                         0 => use no DEBUGger if possible           */
/*                         Ignore on CPNAME='AIPSn','BATERn'          */
/*  In/Out:                                                           */
/*     PID      I(4)       Process identification information:        */
/*                         In:  *(pid + 0) user number for systems    */
/*                                         use it (= 0 otherwise and  */
/*                                         on all AIPSB invocations)  */
/*                         Out: *(pid + 1) process ID number from     */
/*                                         "fork" or "vfork"          */
/*                              *(pid + 2) not used                   */
/*                              *(pid + 3) not used                   */
/*  Output:                                                           */
/*     SYSERR   I          System error code (i.e., errno) if error   */
/*     IERR     I          Error return code: 0 => no error           */
/*                             1 => error                             */
/*  Generic UNIX version.                                             */
/*  If the OS is does not comply with POSIX.1 then ZACTV9 will issue  */
/*  a message and return an error code.                               */
/*--------------------------------------------------------------------*/
                                       /* If _POSIX_VERSION is not    */
                                       /* defined then we do not have */
                                       /* a POSIX.1 system.           */
#ifndef _POSIX_VERSION
{
   printf("ZACTV9 REQUIRES LOCAL DEVELOPMENT\n");
   *ierr = 1;
   return;
}
/*--------------------------------------------------------------------*/
#else
{
   char *pathname;                     /* pathname of executable.     */
   char *dirname;                      /* directory name of           */
   char procname[7];                   /* desired process name.       */
   char rootname[6];                   /* root name of desired        */
                                       /* process (ie. process name   */
                                       /* without the POPS number.    */
   char *debugger;                     /* pathname of debugger.       */
   struct sigaction newaction;         /* Signal action for INT       */
   struct sigaction saved;             /* Previous signal action for  */
                                       /* INT                         */
   char lockname[_POSIX_PATH_MAX];     /* Name of lock file           */
   int  lockfile;                      /* lock file file descriptor   */
   pid_t child;                        /* PID of child process        */
   pid_t grandchild;                   /* PID of grandchild process   */
   int p[2];                           /* File descriptors for pipe   */
   int i, j, n8 = 8;
   char msg[80];
/*--------------------------------------------------------------------*/
   *syserr = 0;
   if (*plen > 6) {
      *ierr = 2;
      return;
      }
                                       /* Convert pathname of         */
                                       /* executable to null-         */
                                       /* terminated form:            */
   pathname = malloc (*flen+1);
   dirname = malloc (*flen+1);
   for (i = 0; i != *flen && fulnam[i] != ' '; i++) {
                                       /* Invariant: for all k,       */
                                       /* 0 <= k < i,                 */
                                       /* pathname[k] = fulnam[k] and */
                                       /* dirname[k] = fulnam[k]      */
      pathname[i] = fulnam[i];
      dirname[i] = fulnam[i];
      if (fulnam[i] == '/') j = i;
      }
   pathname[i] = '\0';
   dirname[j] = '\0';

                                       /* Convert desired process     */
                                       /* name to null-terminated     */
                                       /* form and save rootname:     */
   for (i = 0; i != *plen && cpname[i] != ' '; i++) {
                                       /* Invariant: for all j,       */
                                       /* 0 <= j < i,                 */
                                       /* procname[j] = cpname[j] and */
                                       /* rootname[j] = cpname[j].    */
      procname[i] = cpname[i];
      rootname[i] = cpname[i];
      }
   procname[i] = '\0';
   rootname[i-1] = '\0';

                                       /* Extract the pathname of the */
                                       /* debugger:                   */
   debugger = NULL;
   if (strcmp (rootname, "AIPS") == 0 ||
       strcmp (rootname, "DAIP") == 0) {
      if (getenv ("AIPSDBUGR") != NULL) {
         debugger = malloc (strlen (getenv ("AIPSDBUGR")) + 1);
         strcpy (debugger, getenv ("AIPSDBUGR"));
         }
      }
   else if (strcmp (rootname, "BATER") == 0) {
      if (getenv ("BATERDBUGR") != NULL) {
         debugger = malloc (strlen (getenv ("BATERDBUGR")) + 1);
         strcpy (debugger, getenv ("BATERDBUGR"));
         }
      }
   else if (*idbg == 1) {
      if (getenv ("DBUGR") != NULL) {
         debugger = malloc (strlen (getenv ("DBUGR")) + 1);
         strcpy (debugger, getenv ("DBUGR"));
         }
      }


                                       /* Check that the executable   */
                                       /* exists and that this        */
                                       /* process can execute it.     */
   if (access (pathname, F_OK) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf(msg, "ZACTV9: CANNOT FIND %s", pathname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0) {
         sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
         zmsgwr_ (msg, &n8);
         }
      return;
      }
   if (access (pathname, X_OK) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf(msg, "ZACTV9: NO EXECUTE PERMISSION FOR %s", pathname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0) {
         sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
         zmsgwr_ (msg, &n8);
         }
      return;
      }

   if (debugger != NULL) {
                                       /* Debug mode startup.         */

                                       /* Ignore interrupt signals    */
      newaction.sa_handler = SIG_IGN;
      sigemptyset(&newaction.sa_mask);
      newaction.sa_flags =0;
      if (sigaction (SIGINT, &newaction, &saved) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf(msg, "ZACTV9: CANNOT INSTALL SIGINT HANDLER");
         zmsgwr_ (msg, &n8);
         if (errno >= 0) {
            sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
            zmsgwr_ (msg, &n8);
            }
         return;
         }

                                       /* Check that debugger exists  */
                                       /* and is executable:          */
      if (access (debugger, F_OK) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf(msg, "ZACTV9: CANNOT FIND %s", debugger);
         zmsgwr_ (msg, &n8);
         if (errno >= 0) {
            sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
            zmsgwr_ (msg, &n8);
            }
         goto restore;
         }
      if (access (debugger, X_OK) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf("ZACTV9: NO EXECUTE PERMISSION FOR %s", debugger);
         zmsgwr_ (msg, &n8);
         if (errno >= 0) {
            sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
            zmsgwr_ (msg, &n8);
            }
         goto restore;
         }
                                       /* We need to start the       */
                                       /* program through a link     */
                                       /* otherwise the POPS number  */
                                       /* doesn't propagate through  */
                                       /* the debugger and ZWHOMI    */
                                       /* will fail                  */
      if (chdir (dirname) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf(msg, "ZACTV9: CAN NOT CHANGE DIRECTORY TO %s",
            dirname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0) {
            sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
            zmsgwr_ (msg, &n8);
            }
         goto restore;
         }
      unlink(procname);
      if (link (pathname, procname) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf(msg, "ZACTV9: CAN NOT CREATE LINK TO %s", procname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0) {
            sprintf (msg, "ERRNO = %d (%s)", errno, strerror(errno));
            zmsgwr_ (msg, &n8);
            }
         goto restore;
         }

      if (strcmp (rootname, "AIPS") == 0 ||
          strcmp (rootname, "BATER") == 0 ||
          strcmp (rootname, "DAIP") == 0) {
                                       /* Execute new program in this */
                                       /* process:                    */

         pid[1] = (int)getpid ();
         sprintf (lockname, "/tmp/%s.%d", procname, pid[1]);

                                       /* Create lock file:           */
         if ((lockfile = open (lockname, CMODE, PMODE)) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT CREATE LOCK FILE %s",
               lockname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            goto restore;
            }
         close (lockfile);

         if (execl (debugger, debugger, procname, NULL) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT START DEBUGGER");
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            goto restore;
            }
         }
      else {                           /* Not AIPS or BATER           */
         if ((pid[1] = fork()) == 0) { /* Child process               */

                                       /* Close all open file         */
                                       /* descriptors except for      */
                                       /* anything that might be the  */
                                       /* message terminal:           */
            for (i = 0; i != sysconf(_SC_OPEN_MAX); i++) {
               if ((i != msg_fd) && (i > 2)) {
                  close (i);
                  }
               }


            if (execl (debugger, debugger, procname, NULL) == -1) {
               sprintf(msg, "ZACTV9: CANNOT START %s", procname);
               zmsgwr_ (msg, &n8);
               if (errno >= 0) {
                  sprintf (msg, "ERRNO = %d (%s)", errno,
                     strerror(errno));
                  zmsgwr_ (msg, &n8);
                  }
               exit (1);
               }

            exit (0);
            }
         else if (pid[1] == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT SPAWN CHILD PROCESS");
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            goto restore;
            }
         else {                        /* Still parent                */
                                       /* Create lock file:           */
               sprintf(lockname, "/tmp/%s.%d", procname, pid[1]);
               zmsgwr_ (msg, &n8);
               if ((lockfile = open (lockname, CMODE, PMODE)) == -1) {
                  sprintf(msg, "ZACTV9: CANNOT CREATE LOCK FILE");
                  zmsgwr_ (msg, &n8);
                  if (errno >= 0) {
                     sprintf (msg, "ERRNO = %d (%s)", errno,
                        strerror(errno));
                     zmsgwr_ (msg, &n8);
                     }
                  exit (1);
                  }
               close (lockfile);

                                       /* Wait for child:             */
            waitpid (pid[1], &i, 0);
            }


restore:
         free (debugger);
         unlink (procname);
                                       /* Restore signal handling:    */
         if (sigaction (SIGINT, &saved, &newaction) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT RESTORE SIGNAL HANDLERS");
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            return;
            }
         }
      }
   else {                            /* Non-debug mode            */
      if (strcmp (rootname, "AIPS") == 0 ||
          strcmp (rootname, "BATER") == 0 ||
          strcmp (rootname, "DAIP") == 0) {
                                       /* Execute new program in this */
                                       /* process:                    */

         pid[1] = (int)getpid ();
         sprintf(lockname, "/tmp/%s.%d", procname, pid[1]);

                                       /* Create lock file:           */
         if ((lockfile = open (lockname, CMODE, PMODE)) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT CREATE LOCK FILE %s",
               lockname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            return;
            }
         close (lockfile);

         if (execl (pathname, procname, NULL) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT START %s", procname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            return;
            }
         }
      else {                           /* Not AIPS or BATER           */
                                       /* Spawn new process as a      */
                                       /* grandchild to prevent an    */
                                       /* accumulation of zombies;    */
                                       /* the pid of the grandchild   */
                                       /* will be returned through a  */
                                       /* pipe:                       */

         if (pipe(p) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT CREATE PIPE TO CHILD");
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            return;
            }

         if ((child = fork()) == 0) {  /* Child process               */
            close(p[0]);

                                       /* Close all open file         */
                                       /* descriptors (apart from the */
                                       /* pipe and anything that      */
                                       /* might be the message        */
                                       /* terminal):                  */
            for (i = 0; i != sysconf(_SC_OPEN_MAX); i++) {
               if ((i != p[1]) && (i != msg_fd) && (i > 2)) {
                  close (i);
                  }
               }

            if ((grandchild = fork()) == 0) {
                                       /* Grandchild process          */

                                       /* No need to close file       */
                                       /* descriptors since they were */
                                       /* already closed in the       */
                                       /* immediate parent.           */

               if (execl (pathname, procname, NULL) == -1) {
                  sprintf(msg, "ZACTV9: CANNOT START %s", procname);
                  zmsgwr_ (msg, &n8);
                  if (errno >= 0) {
                     sprintf (msg, "ERRNO = %d (%s)", errno,
                        strerror(errno));
                     zmsgwr_ (msg, &n8);
                     }
                  exit (1);
                  }
               }
            else if (grandchild == -1) {
               sprintf(msg, "ZACTV9: CANNOT SPAWN GRANDCHILD");
               zmsgwr_ (msg, &n8);
               if (errno >= 0) {
                  sprintf (msg, "ERRNO = %d (%s)", errno,
                     strerror(errno));
                  zmsgwr_ (msg, &n8);
                  }
               exit (1);
               }
            else {                     /* Still child                 */
                                       /* Create lock file:           */
               sprintf(lockname, "/tmp/%s.%d", procname, grandchild);
               if ((lockfile = open (lockname, CMODE, PMODE)) == -1) {
                  sprintf(msg, "ZACTV9: CANNOT CREATE LOCK FILE");
                  zmsgwr_ (msg, &n8);
                  if (errno >= 0) {
                     sprintf (msg, "ERRNO = %d (%s)", errno,
                        strerror(errno));
                     zmsgwr_ (msg, &n8);
                     }
                  exit (1);
                  }
               close (lockfile);

                                       /* Return grandchild pid to    */
                                       /* parent process:             */
               if (write (p[1], &grandchild, sizeof(pid_t)) == -1) {
                  sprintf(msg, "ZACTV9: CANNOT SEND PID TO PARENT");
                  zmsgwr_ (msg, &n8);
                  if (errno >= 0) {
                     sprintf (msg, "ERRNO = %d (%s)", errno,
                        strerror(errno));
                     zmsgwr_ (msg, &n8);
                     }
                  exit (1);
                  }
               close (p[1]);

               exit (0);
               }
            }
         else if (child == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf(msg, "ZACTV9: CANNOT SPAWN CHILD PROCESS");
            zmsgwr_ (msg, &n8);
            if (errno >= 0) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                  strerror(errno));
               zmsgwr_ (msg, &n8);
               }
            return;
            }
         else {                        /* Still parent                */
            close(p[1]);

                                       /* Read grandchild pid         */
            if (read (p[0], &grandchild, sizeof(pid_t)) == -1) {
               *syserr = errno;
               *ierr = 1;
               sprintf(msg, "ZACTV9: CANNOT READ PID");
               zmsgwr_ (msg, &n8);
               if (errno >= 0) {
                  sprintf (msg, "ERRNO = %d (%s)", errno,
                     strerror(errno));
                  zmsgwr_ (msg, &n8);
                  }
               return;
               }
            close(p[0]);
            pid[1] = (int)grandchild;

                                       /* Wait for child:             */
            waitpid (child, &i, 0);
            }
         }
      }
   free (pathname);
   free (dirname);
   }

#endif
