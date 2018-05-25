      SUBROUTINE UVDPAD (DISK, CNOSCR, ISCR2, JBUFSZ, BUFF1, BUFF2,
     *   IRET)
C-----------------------------------------------------------------------
C! Reformat UV data record, doubling size and zero extra words.
C# UV Modeling IO-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C-----------------------------------------------------------------------
C   UVDPAD reformats the uv data in the input file by doubling the
C   visibility portion with zero fill of the upper half.  Model values
C   can then be computed and subsequently divided into the observed
C   values by UVDOUT.  The values in /UVHDR/ common (filled by UVPGET)
C   are assumed valid for the input file.  If disk .LE. 0 then CNOSCR
C   is assumed to be the /CFILES/ scratch file number.  Uses LUNs 24
C   and 25 which must not already be in use.
C      If the scratch file ISCR2 does not exist (denoted by ISCR2 .LE.
C   0) then it will be created and entered into /CFILES/ so that it can
C   be destroyed by DIE.
C   Inputs:
C      DISK     I      Disk number for catalogd input file. If .LE. 0
C                      then the input file is a /CFILES/ scratch file.
C      CNOSCR   I      Catalog slot number for if catalogd file;
C                      /CFILES/ scratch file number if a scratch file,
C      ISCR2    I      /CFILES/ number of scratch file for output,
C                      If .LE. 0 then the file will be created.
C      JBUFSZ   I      The size of BUFF1 and BUFF2 in bytes.
C   Inputs from common /UVHDR/:
C      LREC     I      Length of visibility record in R   words.
C      NRPARM   I      Number of random parameters (R   words)
C      NVIS     I      Number of visibilities
C   Output:
C      BUFF1    R(*)   Work buffers
C      BUFF2    R(*)   Work buffers
C      ISCR2    I      /CFILES/ number of output scratch file.
C      IRET     I      Return code, 0 => OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNOSCR, ISCR2, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*)
C
      CHARACTER NAME*48
      INTEGER   VOL, LUNI, LUNO, FINDI, FINDO, BINDI, BINDO, LENBI,
     *   LENBO, INIO, ONIO, LRECO, OCNT, ILOOP, NZERO, BO, VO, ISIZE,
     *   LRECUN, UNPARM, ILOCWT
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNI, LUNO, BO, VO /24,25, 1,0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set input file name.
      IF (DISK.LE.0) THEN
         VOL = SCRVOL(CNOSCR)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCR), 1, NAME, IRET)
      ELSE
         VOL = DISK
         CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
         END IF
C                                       Open input file.
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IRET
         GO TO 990
         END IF
C                                       Setup to init I/O, determine
C                                       size.
      LENBI = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBI, JBUFSZ,
     *   BUFF1, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'READ', IRET
         GO TO 990
         END IF
C                                       If compressed, what would
C                                       uncompressed size be?
      LRECUN = LREC
      UNPARM = NRPARM
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      ILOCWT, IRET)
         IF ((IRET.NE.0) .OR. (ILOCWT.LT.0)) THEN
            IRET = 1
            MSGTXT = 'UVDPAD: CANNOT FIND WEIGHT/SCALE FOR COMPRESSED'
     *         // ' DATA'
            GO TO 990
            END IF
         LRECUN = LREC - NRPARM
         LRECUN = LRECUN * 3
         LRECUN = LRECUN + NRPARM - 2
         UNPARM = NRPARM - 2
C                                       Update SCRHDR data
         SCLREC = 2 * LRECUN - UNPARM
         SCRPRM = LRECUN
         SCRCAT(KINAX) = SCRCAT(KINAX) * 3
         END IF
C                                       Check if scratch file exists,
C                                       if not create it.
      IF (ISCR2.LE.0) THEN
C                                       Create scratch file.
C                                       Must be uncompressed
         CALL UVSIZE ((LRECUN*2 - UNPARM), NVIS, ISIZE)
         CALL SCREAT (ISIZE, BUFF2, IRET)
         ISCR2 = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1020)
            IF (IRET.GT.1) WRITE (MSGTXT,1021) IRET
            GO TO 990
            END IF
         END IF
C                                       Open output file.
      CALL ZPHFIL ('SC', SCRVOL(ISCR2), SCRCNO(ISCR2), 1, NAME, IRET)
      CALL ZOPEN (LUNO, FINDO, SCRVOL(ISCR2), NAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Setup for init.
      LRECO = 2 * LRECUN - UNPARM
      LENBO = 0
      CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LRECO, LENBO, JBUFSZ,
     *   BUFF2, BO, BINDO, IRET)
      OCNT = 0
      ONIO = LENBO
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Set up for reformatting.
      NZERO = LRECUN - UNPARM
C                                       Begin loop
 100     CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO, BINDI, IRET)
         IF (INIO.LE.0) GO TO 250
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Loop copying to output, zero
C                                       padding.
         DO 200 ILOOP = 1,INIO
            IF (COMPDT) THEN
               CALL RCOPY (UNPARM, BUFF1(BINDI), BUFF2(BINDO))
               CALL ZUVXPN ((LREC-NRPARM), BUFF1(BINDI+NRPARM),
     *            BUFF1(BINDI+ILOCWT), BUFF2(BINDO+UNPARM))
            ELSE
               CALL RCOPY (LREC, BUFF1(BINDI), BUFF2(BINDO))
               END IF
C                                       Zero fill upper portion.
            CALL RFILL (NZERO, 0.0, BUFF2(BINDO+LRECUN))
C                                       Update pointers.
            BINDI = BINDI + LREC
            BINDO = BINDO + LRECO
C                                       Check if time for output.
            OCNT = OCNT + 1
C                                       Write
            IF (OCNT.GE.ONIO) THEN
               ONIO = OCNT
               CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, ONIO, BINDO,
     *            IRET)
               OCNT = 0
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
 200        CONTINUE
C                                       Loop back for more.
         GO TO 100
C                                       Done - flush output buffer.
 250  ONIO = -OCNT
      CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, ONIO, BINDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IRET
         GO TO 990
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDPAD: OPEN FOR ',A4,' ERROR ',I5)
 1010 FORMAT ('UVDPAD: INIT FOR ',A4,' ERROR ',I5)
 1020 FORMAT ('UVDPAD: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1021 FORMAT ('UVDPAD: ERROR ',I5,' CREATING SCRATCH FILE')
 1100 FORMAT ('UVDPAD: ',A4,' ERROR ',I5)
      END
