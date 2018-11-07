      PROGRAM ICON
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2012
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
C   ICON converts icon file formats
C   It changes a file in 'iconedit' format to the format needed by
C   XAS for XCreateBitmapFromData.  Namely, it changes from 16-bit
C   data to 8-bit data and reverses the order of the bits in each
C   8-bit datum.
C-----------------------------------------------------------------------
      INTEGER   TTY(2), IERR, ILUN, OLUN, IFIND, OFIND, JTRIM, IL, I, JT
      CHARACTER INFIL*48, OUTFIL*48, ILINE*80, MSGBUF*80, OLINE*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN, OLUN /10,11/
C-----------------------------------------------------------------------
      TTY(1) = 5
      CALL AIPINI (TTY, 'ICON  ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       infile name
      WRITE (MSGBUF, 1000) 'input'
      CALL INQSTR (TTY, MSGBUF, 48, INFIL, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF, 1000) 'output'
      CALL INQSTR (TTY, MSGBUF, 48, OUTFIL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       output file name
      CALL ZTXOPN ('READ', ILUN, IFIND, INFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'open', 'input', IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       open files
      CALL ZTXOPN ('WRIT', OLUN, OFIND, OUTFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'open', 'output', IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       copy beginning comment lines
      DO 10 I = 1,2
         CALL ZTXIO ('READ', ILUN, IFIND, ILINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'read', 'input', IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
         IL = JTRIM (ILINE)
         OLINE = ILINE
         CALL ZTXIO ('WRIT', OLUN, OFIND, OLINE(:IL), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'writ', 'output', IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
 10      CONTINUE
C                                       copy 64 lines of icon
      DO 50 I = 1,64
         CALL ZTXIO ('READ', ILUN, IFIND, ILINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'read', 'input', IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
         JT = JTRIM (ILINE)
C                                       output form
         IF (I.EQ.1) THEN
C                                       1st char = TAB
            OLINE = ILINE(:1)
            OLINE(2:3) = '0x'
            OLINE(6:8) = ',0x'
            OLINE(11:13) = ',0x'
            OLINE(16:18) = ',0x'
            OLINE(21:23) = ',0x'
            OLINE(26:28) = ',0x'
            OLINE(31:33) = ',0x'
            OLINE(36:38) = ',0x'
            OLINE(41:) = ','
            IL = 41
            END IF
C                                       reverse bits in bytes
         CALL REVCHR (ILINE(4:), OLINE(4:))
         CALL REVCHR (ILINE(6:), OLINE(9:))
         CALL REVCHR (ILINE(11:), OLINE(14:))
         CALL REVCHR (ILINE(13:), OLINE(19:))
         CALL REVCHR (ILINE(18:), OLINE(24:))
         CALL REVCHR (ILINE(20:), OLINE(29:))
         CALL REVCHR (ILINE(25:), OLINE(34:))
         CALL REVCHR (ILINE(27:), OLINE(39:))
C                                       write it out
         IF (I.EQ.64) IL = 40
         CALL ZTXIO ('WRIT', OLUN, OFIND, OLINE(:IL), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'writ', 'output', IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
 50      CONTINUE
C                                       close files
      CALL ZTXCLS (OLUN, OFIND, IERR)
      CALL ZTXCLS (ILUN, IFIND, IERR)
C                                       exit gracefully
 980  CALL ZCLOSE (TTY(1), TTY(2), IERR)
      CALL ACOUNT (2)
C
 990  CONTINUE
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter ',A,' file name as logical:name')
 1010 FORMAT ('ERROR ON ',A,' TO ',A,' NUMBER',I6)
      END
      SUBROUTINE REVCHR (IC, OC)
C-----------------------------------------------------------------------
C   reverse bits in 2-character bytes
C   Input:
C      IC   C*(*)   Use 1st 2 chars only
C   Output:
C      OC   C*(*)   !st 2 chars changed to bit reverse of IC(1:2)
C-----------------------------------------------------------------------
      CHARACTER IC*(*), OC*(*)
C
      CHARACTER LC*1
      INTEGER   I, I1, I2
C-----------------------------------------------------------------------
      DO 10 I = 1,2
         I1 = I
         I2 = 3 - I
         LC = IC(I1:I1)
         IF (LC.EQ.'0') THEN
            OC(I2:I2) = '0'
         ELSE IF (LC.EQ.'1') THEN
            OC(I2:I2) = '8'
         ELSE IF (LC.EQ.'2') THEN
            OC(I2:I2) = '4'
         ELSE IF (LC.EQ.'3') THEN
            OC(I2:I2) = 'C'
         ELSE IF (LC.EQ.'4') THEN
            OC(I2:I2) = '2'
         ELSE IF (LC.EQ.'5') THEN
            OC(I2:I2) = 'A'
         ELSE IF (LC.EQ.'6') THEN
            OC(I2:I2) = '6'
         ELSE IF (LC.EQ.'7') THEN
            OC(I2:I2) = 'E'
         ELSE IF (LC.EQ.'8') THEN
            OC(I2:I2) = '1'
         ELSE IF (LC.EQ.'9') THEN
            OC(I2:I2) = '9'
         ELSE IF (LC.EQ.'A') THEN
            OC(I2:I2) = '5'
         ELSE IF (LC.EQ.'B') THEN
            OC(I2:I2) = 'D'
         ELSE IF (LC.EQ.'C') THEN
            OC(I2:I2) = '3'
         ELSE IF (LC.EQ.'D') THEN
            OC(I2:I2) = 'B'
         ELSE IF (LC.EQ.'E') THEN
            OC(I2:I2) = '7'
         ELSE IF (LC.EQ.'F') THEN
            OC(I2:I2) = 'F'
            END IF
 10      CONTINUE
C
 999  RETURN
      END
