      SUBROUTINE CENTSU (DISK, CNO, DIFPIX, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Correct SU velocity values for centering frequency axis
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   Corrects FQ table ref pixel frequencies for a change in ref pixel
C   Inputs:
C     DISK     I      Volume number
C     CNO      I      Catalog number
C     DIFPIX   R      Pixel shift
C   Input (via common DUVH, MAPHDR)
C     JLOCF    I      frequency axis pointer
C     CATBLK   I(*)   output image header
C   Outputs:
C     IRET     I      Error code, 0 => OK, > 0 => FQ table error
C-----------------------------------------------------------------------
      INTEGER  DISK, CNO, BUFF1(*), BUFF2(*), IRET
      REAL     DIFPIX
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMIF, FREQID, ISURNO, SUKOLS(MAXSUC), SUNUMV(MAXSUC),
     *   IDSOU, QUAL, I, VER, LUNI, LUNO, LUNTMP, NUMSU, IIF, MSGSAV,
     *   NVER
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA,
     *   PMDEC, NUX, FRQREF, REFFRQ, FRQDIF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (DIFPIX.EQ.0.0) GO TO 999
      CALL FNDEXT ('SU', CATBLK, I)
      IF (I.LE.0) GO TO 999
C                                       Open FQ table
      VER = 1
      LUNI = LUNTMP (1)
      CALL SOUINI ('READ', BUFF1, DISK, CNO, VER, CATBLK, LUNI, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT SU TABLE'
         GO TO 990
         END IF
      NUMSU = BUFF1(5)
C                                       is velocities not set, quit
      IF ((VELDEF.NE.'RADIO') .AND. (VELDEF.NE.'OPTICAL')) GO TO 995
C                                       output
      LUNO = LUNTMP (1)
C                                       get FQ settings first
      CALL CHNDAT ('READ', BUFF2, DISK, CNO, VER, CATBLK, LUNO, NUMIF,
     *   FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE FOR VELOCITIES'
         GO TO 990
         END IF
      NVER = 0
      CALL SOUINI ('WRIT', BUFF2, DISK, CNO, NVER, CATBLK, LUNO, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT SU TABLE'
         GO TO 990
         END IF
      REFFRQ = CATD(KDCRV+JLOCF)
C
      DO 100 I = 1,NUMSU
         ISURNO = I
         CALL TABSOU ('READ', BUFF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT SU TABLE'
            GO TO 990
            END IF
         DO 20 IIF = 1,NUMIF
            NUX = REFFRQ + FOFF(IIF) + FREQO(IIF)
            FRQREF = -DIFPIX * FINC(IIF) + NUX
C                                       optical - non linear
            IF (VELDEF.EQ.'OPTICAL') THEN
               FRQDIF = LRESTF(IIF) * (1.0D0/NUX - 1.0D0/FRQREF)
               LSRVEL(IIF) = LSRVEL(IIF) + VELITE * FRQDIF
C                                       radio is linear
            ELSE
               FRQDIF = NUX - FRQREF
               LSRVEL(IIF) = - FRQDIF / LRESTF(IIF) * VELITE +
     *            LSRVEL(IIF)
               END IF
 20         CONTINUE
C                                       write
         ISURNO = I
         CALL TABSOU ('WRIT', BUFF2, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT SU TABLE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close output table
      CALL TABIO ('CLOS', 0, ISURNO, BUFF2, BUFF2, I)
      CALL TABIO ('CLOS', 0, ISURNO, BUFF1, BUFF1, I)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'SU', VER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DELETE ORIGINAL'
         GO TO 990
         END IF
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('SU', NVER, VER, LUNO, LUNI, DISK, DISK, CNO, CNO,
     *   CATBLK, BUFF1, BUFF2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'COPYING TEMP TO OLD'
         GO TO 990
         END IF
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'SU', NVER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DELETE TEMP'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C                                       close input table
 995  CALL TABIO ('CLOS', 0, ISURNO, BUFF1, BUFF1, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CENTSU ERROR:',I5,' ON ',A)
      END
