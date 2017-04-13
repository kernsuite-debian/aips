#include <stdio.h>
#include <signal.h>
#include <errno.h>

#define PMODE  0666                     /* File creation mode bits.   */
#define MAXPATH 128                     /* Maximum path name length.  */

zactv9_(flen, fulnam, plen, cpname, idbg, pid, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! starts requested load module under requested process name         */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*  ZACTV9 is only called by ZACTV8.                                  */
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
/*--------------------------------------------------------------------*/
int *flen, *plen, *idbg, *pid, *syserr, *ierr;
char fulnam[], cpname[];
/*--------------------------------------------------------------------*/
{
   void (*onint)();
   extern int errno;
   int n8 = 8;
   char msg[80];
   register int i, j;
   extern char *sys_errlist[];         /* System error messages.      */
   extern int sys_nerr;                /* Number of system error      */
                                       /* messages.                   */
   static int child[32];                /* More than enough birth     */
                                        /* certificates for even the  */
                                        /* most prolific AIPS user.   */
   int kicking, fd;
   char pathname[MAXPATH], dirname[MAXPATH], linkname[MAXPATH],
      procname[7], rootname[7], lockname[MAXPATH], debugger[MAXPATH];
/*--------------------------------------------------------------------*/
                                        /* Scheme to clean up zombie  */
                                        /* processes.  These are      */
                                        /* terminated child processes */
                                        /* for which we did not       */
                                        /* "wait" (AIPS has its       */
                                        /* own waiting mechanism).    */
                                        /* Zombies hang around        */
                                        /* indefinitely, taking up a  */
                                        /* precious process slot      */
                                        /* (typically max = 25/user)  */
                                        /* until either their parent  */
                                        /* waits or terminates.       */
   kicking = 0;
   for (i = 0; i < 32; i++) {
      if (child[i] != 0 && kill(child[i],0) == 0) {
                                        /* One still kicking from     */
                                        /* earlier "fork".            */
         kicking = 1;
         break;
         }
      }
                                        /* If any children are still  */
                                        /* kicking, don't "wait".     */
   if (kicking == 0) {
                                        /* Otherwise, drive stakes    */
                                        /* through hearts of any      */
                                        /* zombied children.          */
      for (i = 0; i < 32; i++) {
         if (child[i] != 0) {
                                        /* Wait.  Return should be    */
                                        /* immediate since all        */
                                        /* children are supposedly    */
                                        /* dead.                      */
            wait(0);
                                        /* Recycle birth certificate. */
            child[i] = 0;
            }
         }
      }
                                        /* Assume success.            */
   *ierr = 0;
   errno = 0;
   *(pid + 1) = 0;
                                        /* Find last non-blank        */
                                        /* character in "fulnam" as   */
                                        /* well as the last occurence */
                                        /* of "/".  Form null         */
                                        /* terminated pathname of the */
                                        /* desired executable module. */
   for (i = 0; i < *flen && fulnam[i] != ' '; i++) {
      if (fulnam[i] == '/') j = i;
      pathname[i] = fulnam[i];
      }
   pathname[i] = '\0';
                                        /* Extract null terminated    */
                                        /* directory name from        */
                                        /* "pathname".                */
   for (i = 0; i < j; i++)
      dirname[i] = pathname[i];
   dirname[i] = '\0';
                                        /* Form null terminated       */
                                        /* AIPS process name and root */
                                        /* name from "cpname" (root   */
                                        /* name is just process name  */
                                        /* without POPS number        */
                                        /* suffix).                   */
   for (i = 0; i < *plen && cpname[i] != ' '; i++) {
      procname[i] = cpname[i];
      rootname[i] = cpname[i];
      }
   procname[i] = '\0';
   rootname[i-1] = '\0';
                                        /* Form null terminated link  */
                                        /* name out of "pathname"     */
                                        /* plus "procname".           */
   sprintf (linkname, "%s/%s", dirname, procname);
                                        /* Remove any extant          */
                                        /* "linkname".                */
   unlink (linkname);
                                        /* Extract value of debugger  */
                                        /* environment variable       */
                                        /* (different for AIPS and    */
                                        /* BATER versus tasks).  If   */
                                        /* non-null, its value        */
                                        /* should be the pathname of  */
                                        /* a legitimate debugger, e.g,*/
                                        /* /bin/adb, which will be    */
                                        /* invoked and under whose    */
                                        /* control the program will   */
                                        /* run.                       */
   debugger[0] = '\0';
   if (strcmp (rootname, "AIPS") == 0) {
      if (getenv ("AIPSDBUGR") != NULL)
         sprintf (debugger, "%s", getenv ("AIPSDBUGR"));
      }
   else if (strcmp (rootname, "BATER") == 0) {
      if (getenv ("BATERDBUGR") != NULL)
         sprintf (debugger, "%s", getenv ("BATERDBUGR"));
      }
   else if (*idbg == 1) {
      if (getenv ("DBUGR") != NULL)
         sprintf (debugger, "%s", getenv ("DBUGR"));
      }
                                        /* Check for existence of     */
                                        /* desired executable module. */
   if (access (pathname, 0) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf (msg, "ZACTV9: Can't find %s", pathname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0 && errno <= sys_nerr) {
         sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
         zmsgwr_ (msg, &n8);
         }
      }
                                        /* Is it executable?          */
   else if (access (pathname, 1) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf (msg, "ZACTV9: %s is not executable", pathname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0 && errno <= sys_nerr) {
         sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
         zmsgwr_ (msg, &n8);
         }
      }
                                        /* Form hard link between     */
                                        /* "pathname" of desired      */
                                        /* executable module and      */
                                        /* "linkname".                */
   else if (link (pathname, linkname) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf (msg, "ZACTV9: Trouble with link(%s,%s)", pathname,
         linkname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0 && errno <= sys_nerr) {
         sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
         zmsgwr_ (msg, &n8);
         }
      }
                                        /* Change to the directory    */
                                        /* "dirname".                 */
   else if (chdir (dirname) != 0) {
      *syserr = errno;
      *ierr = 1;
      sprintf (msg, "ZACTV9: Trouble changing to directory %s",
         dirname);
      zmsgwr_ (msg, &n8);
      if (errno >= 0 && errno <= sys_nerr) {
         sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
         zmsgwr_ (msg, &n8);
         }
      }
                                        /* Now we take one of several */
                                        /* actions:                   */

                                        /* 1) If the value of         */
                                        /* "debugger" is non-null     */
                                        /* we invoke the specified    */
                                        /* debugger with suitable     */
                                        /* arguments.                 */

                                        /* 2) For certain programs    */
                                        /* (i.e., AIPS and BATER) we  */
                                        /* "execl" w/o "vfork/fork"   */
                                        /* such that ZSTRTA and       */
                                        /* ZSTRTB get replaced by     */
                                        /* AIPSx and BATERx.  This    */
                                        /* re-uses the process slots  */
                                        /* that would otherwise be    */
                                        /* taken up by the ZSTRT*     */
                                        /* programs.  Most UNIX       */
                                        /* UNIX systems impose a      */
                                        /* user-based limit of 25     */
                                        /* processes that is not easy */
                                        /* to overcome.               */

                                        /* In all cases, rename the   */
                                        /* process "procname" and     */
                                        /* create a lock file in /tmp */
                                        /* with a name formed from    */
                                        /* "procname" + *(pid + 1).   */
                                        /* Process renaming is still  */
                                        /* needed for the slow "SPY"  */
                                        /* facility (ZXTSPY), whereas */
                                        /* the lock files allow for   */
                                        /* a very efficient as well   */
                                        /* as robust implementations  */
                                        /* of task querying (ZTACT2)  */
                                        /* and task killing (ZTKILL). */

                                        /* Debug mode.                */
   else if (debugger[0] != '\0') {
                                        /* Suspend interrupt          */
                                        /* processing.                */
      onint = signal (SIGINT, SIG_IGN);
                                        /* Check for existence of     */
                                        /* specified debugger.        */
      if (access (debugger, 0) != 0) {
         *syserr = errno;
         *ierr = 1;
         sprintf (msg, "ZACTV9: Can't find %s", debugger);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno,
              sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
                                        /* Is it executable?          */
      else if (access (debugger, 1) != 0) {
         *syserr = errno;
         *ierr = 1;
         sprintf (msg, "ZACTV9: %s is not executable", debugger);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno,
              sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
                                        /* Is "rootname" AIPS (i.e.,  */
                                        /* as from ZSTRTA) or BATER   */
                                        /* (i.e., as from ZSTRTB)?    */
      else if (strcmp (rootname, "AIPS") == 0 ||
         strcmp (rootname, "BATER") == 0) {
                                        /* "Execl" without fork =>    */
                                        /* core image of ZSTRT* will  */
                                        /* be replaced by the core    */
                                        /* image of AIPS (or BATER).  */
                                        /* Also, process id will be   */
                                        /* the same as the current    */
                                        /* process.                   */
         *(pid + 1) = getpid ();
                                        /* Form null terminated lock  */
                                        /* name out of procname plus  */
                                        /* the *(pid + 1).            */
         sprintf (lockname, "/tmp/%s.%d", procname, *(pid + 1));
                                        /* Create lock file.          */
         if ((fd = creat (lockname, PMODE)) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf (msg, "ZACTV9: Trouble creating %s", lockname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0 && errno <= sys_nerr) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                 sys_errlist[errno]);
               zmsgwr_ (msg, &n8);
               }
            }
                                        /* Close lock file.           */
         else close (fd);
                                        /* Invoke "debugger" as       */
                                        /* process named "procname".  */
         if (execl (debugger, procname, procname, NULL) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf (msg, "ZACTV9: trouble executing debugger %s as %s",
               debugger, procname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0 && errno <= sys_nerr) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                 sys_errlist[errno]);
               zmsgwr_ (msg, &n8);
               }
            }
         }
                                        /* Spawn new process (some    */
                                        /* systems may not have       */
                                        /* "vfork"; use "fork"        */
                                        /* instead).                  */
      else if ((*(pid + 1) = vfork ()) == 0) {
                                        /* Invoke "debugger" as       */
                                        /* process named "procname".  */
         if (execl (debugger, procname, procname, NULL) == -1) {
            *syserr = errno;
            *ierr = 1;
            sprintf (msg, "ZACTV9: trouble executing debugger %s as %s",
               debugger, procname);
            zmsgwr_ (msg, &n8);
            if (errno >= 0 && errno <= sys_nerr) {
               sprintf (msg, "ERRNO = %d (%s)", errno,
                 sys_errlist[errno]);
               zmsgwr_ (msg, &n8);
               }
            }
         }
                                        /* Form null terminated lock  */
                                        /* name out of procname plus  */
                                        /* the *(pid + 1).            */
      sprintf (lockname, "/tmp/%s.%d", procname, *(pid + 1));
                                        /* Create lock file.          */
      if ((fd = creat (lockname, PMODE)) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf (msg, "ZACTV9: Trouble creating %s", lockname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno,
              sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
                                        /* Close lock file.           */
      else close (fd);
      }
                                        /* Non-debug mode.            */

                                        /* Is "rootname" AIPS (i.e.,  */
                                        /* as from ZSTRTA) or BATER   */
                                        /* (i.e., as from ZSTRTB)?    */
   else if (strcmp (rootname, "AIPS") == 0 ||
      strcmp (rootname, "BATER") == 0) {
                                        /* "Execl" without fork =>    */
                                        /* core image of ZSTRT* will  */
                                        /* be replaced by the core    */
                                        /* image of AIPS (or BATER).  */
                                        /* Also, process id will be   */
                                        /* the same as the current    */
                                        /* process.                   */
      *(pid + 1) = getpid ();
                                        /* Form null terminated lock  */
                                        /* name out of procname plus  */
                                        /* the *(pid + 1).            */
      sprintf (lockname, "/tmp/%s.%d", procname, *(pid + 1));
                                        /* Create lock file.          */
      if ((fd = creat (lockname, PMODE)) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf (msg, "ZACTV9: Trouble creating %s", lockname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
                                        /* Close lock file.           */
      else close (fd);
                                        /* Invoke "pathname" as       */
                                        /* process named "procname".  */
      if (execl (pathname, procname, NULL) == -1) {
         *syserr = errno;
         *ierr = 1;
         sprintf (msg, "ZACTV9: trouble executing pathname %s as %s",
         pathname, procname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
      }
                                        /* Spawn new process (some    */
                                        /* systems may not have       */
                                        /* "vfork"; use "fork"        */
                                        /* instead).                  */
   else if ((*(pid + 1) = vfork()) == 0) {
                                        /* Invoke "pathname" as       */
                                        /* process named "procname".  */
      if (execl (pathname, procname, NULL) == -1) {
         *syserr = errno;
         *ierr = 0;
         sprintf (msg, "ZACTV9: trouble executing pathname %s as %s",
            pathname, procname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
      }
                                        /* Test for success.          */
   if (*(pid + 1) <= 0)
      *ierr = 1;
   else {
                                        /* Form null terminated lock  */
                                        /* name out of procname plus  */
                                        /* the *(pid + 1).            */
      sprintf (lockname, "/tmp/%s.%d", procname, *(pid + 1));
                                        /* Create lock file.          */
      if ((fd = creat (lockname, PMODE)) == -1) {
         *syserr = errno;
         *ierr = 1;
         printf ("ZACTV9: Trouble creating %s", lockname);
         zmsgwr_ (msg, &n8);
         if (errno >= 0 && errno <= sys_nerr) {
            sprintf (msg, "ERRNO = %d (%s)", errno, sys_errlist[errno]);
            zmsgwr_ (msg, &n8);
            }
         }
                                        /* Close lock file.           */
      else close (fd);
      }
                                        /* If debug mode, wait.       */
   if (debugger[0] != '\0') {
      wait (0);
                                        /* Restore interrupt          */
                                        /* processing.                */
      signal (SIGINT, onint);
      }
   else {
                                        /* Find blank birth           */
                                        /* certificate.               */
      for (i = 0; i < 32 && child[i] != 0; i++);
                                        /* Fill it with process id of */
                                        /* newborn.                   */
      if (i < 32) child[i] = *(pid + 1);
      }
                                        /* Remove link file (success  */
                                        /* or not).                   */
   unlink (linkname);
                                        /* Exit.                      */
   return;
}
