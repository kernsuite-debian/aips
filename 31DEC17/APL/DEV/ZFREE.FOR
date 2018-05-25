      SUBROUTINE ZFREE (MSLEV, IERR)
C-----------------------------------------------------------------------
C! display available disk space
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003-2004, 2008-2009, 2017
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Determine the number of free 256-integer blocks that are available
C   on the disks used for AIPS user data and print the information on
C   the user's terminal.
C   Inputs:
C      MSLEV   I   Message level to use (1 => to terminal only,
C                     2-5 => also to message file)
C   Common /DCHCOM/
C      NVOL    I   Number of AIPS disks
C   Output:
C     IERR     I   Error return code: 0 => no error
C                     1 => error
C   UNIX version - uses ZFRE2 to get info then string processes with
C   UNIX file names in mind.
C-----------------------------------------------------------------------
      INTEGER   MSLEV, IERR
C
      INTEGER   I, VLEN, TOTBLK(99), FRBLKS(99), J1, J2, IP, J, ITRIM,
     *   MXBLK, NC, IC, K
      REAL      PCTFUL(99)
      HOLLERITH HVOLN(1200)
      CHARACTER ACCES*7, LVOL*64
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSLEV = MAX (1, MIN (9, MSLEV))
      IERR = 0
C                                       Get AIPS disk info
      LVOL = ' '
      CALL CHR2H (4800, LVOL, 1, HVOLN)
      VLEN = 48
      CALL ZFRE2 (NVOL, HVOLN, VLEN, TOTBLK, PCTFUL, FRBLKS)
C                                       Find maxima
      MXBLK = 0
      DO 10 I = 1,NVOL
         MXBLK = MAX (MXBLK, TOTBLK(I))
 10      CONTINUE
C                                       format
      IF (MXBLK.GE.1000000000) THEN
         IC = 12
      ELSE IF (MXBLK.GE.100000000) THEN
         IC = 11
      ELSE IF (MXBLK.GE.10000000) THEN
         IC = 10
      ELSE
         IC = 9
         END IF
      NC = 42 - 2*IC
C                                       Print header.
      MSGTXT = 'Disk Volume name'
      J = 4 + NC + IC - 4
      MSGTXT(J:) = 'Total Full'
      J = 4 + NC + IC + 5 + IC - 3
      MSGTXT(J:) = 'Free Timd Access'
      CALL MSGWRT (MSLEV)
      MSGTXT = ' #'
      K = 4 + NC + IC - 5
      MSGTXT(K:) = 'Mbytes    %'
      K = 4 + NC + IC + 5 + IC - 5
      MSGTXT(K:) = 'Mbytes days'
      CALL MSGWRT (MSLEV)
C                                       Print info
      DO 40 I = 1,NVOL
         J2 = I * VLEN
         J1 = J2 - VLEN + 1
         CALL H2CHR (VLEN, J1, HVOLN, LVOL)
         J = ITRIM (LVOL)
         IF (J.GT.NC) THEN
            J2 = J1 + J - 1
            J1 = J2 - NC + 1
            J = J2 - J1 + 1
            CALL H2CHR (J, J1, HVOLN, LVOL)
            LVOL(1:1) = '<'
            END IF
         IP = PCTFUL(I) + 0.5
         ACCES = 'Alluser'
         IF (DASSGN(1,I).NE.0) THEN
            IF (DASSGN(1,I).LT.0) THEN
               ACCES = 'Scratch'
            ELSE
               ACCES = 'Not you'
               DO 30 J = 1,8
                  IF (DASSGN(J,I).EQ.NLUSER) ACCES = 'Resrved'
 30               CONTINUE
               IF (NLUSER.EQ.1) ACCES = 'Resrved'
               END IF
            END IF
         J = TIMEDA(I) + 0.5
         IF (IC.EQ.9) THEN
            WRITE (MSGTXT,1030,ERR=35) I, LVOL(:NC), TOTBLK(I), IP,
     *         FRBLKS(I), J, ACCES
         ELSE IF (IC.EQ.10) THEN
            WRITE (MSGTXT,1031,ERR=35) I, LVOL(:NC), TOTBLK(I), IP,
     *         FRBLKS(I), J, ACCES
         ELSE IF (IC.EQ.11) THEN
            WRITE (MSGTXT,1032,ERR=35) I, LVOL(:NC), TOTBLK(I), IP,
     *         FRBLKS(I), J, ACCES
         ELSE
            WRITE (MSGTXT,1033,ERR=35) I, LVOL(:NC), TOTBLK(I), IP,
     *         FRBLKS(I), J, ACCES
            END IF
 35      CALL MSGWRT (MSLEV)
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I2,2X,A,I9,I5,I9,I5,1X,A7)
 1031 FORMAT (I2,2X,A,I10,I5,I10,I5,1X,A7)
 1032 FORMAT (I2,2X,A,I11,I5,I11,I5,1X,A7)
 1033 FORMAT (I2,2X,A,I12,I5,I12,I5,1X,A7)
      END
