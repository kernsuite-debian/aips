      SUBROUTINE UVDOUT (ISCR1, DISK, CNOSCR, CHANEL, NCHAN, JBUFSZ,
     *   BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Divides uv model in one half of a record into other, writes result.
C# UV Modeling IO-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004, 2015
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
C   UVDOUT works on visibility data in which the lower half has
C   observed values and the upper half, in identical form, has model
C   values.  The model values are divided into the observed values and
C   only the ratios are written to the output.
C      The weight of the output record is multiplied by the amplitude
C   of the model visibility.
C      The contents of /UVHDR/ are assumed valid for the output.
C      If disk .LE. 0 then CNOSCR is assumed to be the /CFILES/ scratch
C   file number.  Uses LUNs 24 and 25.
C   Inputs:
C      ISCR1    I      /CFILES/ number of scratch file for input
C      DISK     I      Disk number for catalogd output file. If .LE. 0
C                      then the output file is a /CFILES/ scratch file.
C      CNOSCR   I      Catalog slot number if catalogd output file;
C                      /CFILES/ scratch file number if a scratch file,
C      CHANEL   I      First uv data channel to divide.
C      NCHAN    I      Number of model channels to divide.
C      JBUFSZ   I      The size of BUFF1 and BUFF2 in bytes.
C   Inputs from common /UVHDR/:
C      LREC     I      Length of visibility record in words.
C      NRPARM   I      Number of random parameters
C      NVIS     I      Number of visibilities
C   Output:
C      BUFF1    R(*)   Work buffers
C      BUFF2    R(*)   Work buffers
C      ISCR2    I      /CFILES/ number of output scratch file.
C      IRET     I      Return code, 0 => OK, otherwise failed.
C-----------------------------------------------------------------------
      REAL      BUFF1(*), BUFF2(*)
      INTEGER   ISCR1, DISK, CNOSCR, CHANEL, NCHAN, JBUFSZ, IRET
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*48
      INTEGER   VOL, FINDI, FINDO, KLOOP, LUNI, LUNO, BINDI, BINDO,
     *   LENBO, INIO, ONIO, LRECI, VVADD, OCNT, ILOOP, INCVIS, JLOOP,
     *   LENBI, IPM, IPO, OPO, IIPM, IIPO, OOPO, BO, VO, LDATA,
     *   LRECUN, UNPARM
      LOGICAL   T, F
      REAL      AMP, AMPI, RATRE, RATIM, WT, TEMPBF(UVBFSS), WTSCL(2)
      SAVE TEMPBF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA LUNI, LUNO, BO, VO /24,25, 1,0/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set output file name.
      IF (DISK.LE.0) THEN
         VOL = SCRVOL(CNOSCR)
         CALL ZPHFIL ('SC', SCRVOL(CNOSCR), SCRCNO(CNOSCR), 1, NAME,
     *      IRET)
      ELSE
         VOL = DISK
         CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUNO, FINDO, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Setup to init I/O, determine
C                                       size.
      LENBO = 0
      CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LREC, LENBO, JBUFSZ,
     *   BUFF2, BO, BINDO, IRET)
      OCNT = 0
      ONIO = LENBO
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Open input file.
      CALL ZPHFIL ('SC', SCRVOL(ISCR1), SCRCNO(ISCR1), 1, NAME, IRET)
      CALL ZOPEN (LUNI, FINDI, SCRVOL(ISCR1), NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IRET
         GO TO 990
         END IF
C                                       Setup for init.
      LRECI = 2 * SCLREC - SCRPRM
      IF (COMPDT) LRECI = SCLREC
      LENBI = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LRECI, LENBI, JBUFSZ,
     *   BUFF1, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'READ', IRET
         GO TO 990
         END IF
C                                       Set up for reformatting.
      INCVIS = MIN (INCS, INCF)
      INCVIS = MAX (2, INCVIS)
      VVADD = (CHANEL - 1) * INCF + VOFF
      LRECUN = LREC
      UNPARM = NRPARM
      IF (COMPDT) THEN
         LRECUN = LREC - NRPARM
         LRECUN = LRECUN * 3
         LRECUN = LRECUN + NRPARM - 2
         UNPARM = NRPARM - 2
         END IF
      LDATA = (LRECUN - UNPARM) / INCS
      IF (LDATA.GT.UVBFSS) THEN
         MSGTXT = 'UVDOUT: TEMPORARY BUFFER TOO SMALL - MODIFY'
         IRET = 1
         GO TO 990
         END IF
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
C                                       Copy random parameters and obs.
            CALL RCOPY (LRECUN, BUFF1(BINDI), BUFF2(BINDO))
            IIPO = BINDI + UNPARM + VVADD
            IIPM = BINDI + LRECUN + VVADD
            OOPO = BINDO + UNPARM + VVADD
C                                       Divide
C                                       Loop over channel.
            DO 150 JLOOP = 1,NCHAN
C                                       Set indices.
               IPO = IIPO
               IPM = IIPM
               OPO = OOPO
C                                       Loop over stokes'
               DO 130 KLOOP = 1,NSTOK
                  AMP = BUFF1(IPM)*BUFF1(IPM) +
     *               BUFF1(IPM+1)*BUFF1(IPM+1)
                  RATRE = 0.0
                  RATIM = 0.0
                  WT = 0.0
C                                       Check for small values.
                  IF (AMP.LT.1.0E-20) GO TO 120
                     AMPI = 1.0 / AMP
                     RATRE = AMPI * ( BUFF1(IPO)*BUFF1(IPM) +
     *                  BUFF1(IPO+1)*BUFF1(IPM+1))
                     RATIM = AMPI * ( BUFF1(IPM)*BUFF1(IPO+1) -
     *                  BUFF1(IPO)*BUFF1(IPM+1))
                     WT = AMP * BUFF1(IPO+2)
C                                       Fill in.
 120              BUFF2(OPO) = RATRE
                  BUFF2(OPO+1) = RATIM
                  BUFF2(OPO+2) = WT
C                                       Update pointers for stokes'
                  IPO = IPO + INCS
                  IPM = IPM + INCS
                  OPO = OPO + INCS
 130              CONTINUE
C                                       Update pointers for freq.
               IIPO = IIPO + INCF
               IIPM = IIPM + INCF
               OOPO = OOPO + INCF
 150           CONTINUE
C                                       Compress data if needed
            IF (COMPDT) THEN
               CALL ZUVPAK (LDATA, BUFF2(BINDO+UNPARM),
     *            WTSCL,  TEMPBF)
               CALL RCOPY (2, WTSCL, BUFF2(BINDO+UNPARM))
               CALL RCOPY (LDATA, TEMPBF, BUFF2(BINDO+UNPARM+2))
               END IF
C                                       Update pointers.
            BINDI = BINDI + LRECI
            BINDO = BINDO + LREC
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
 1000 FORMAT ('UVDOUT: OPEN FOR ',A4,' ERROR ',I5)
 1010 FORMAT ('UVDOUT: INIT FOR ',A4,' ERROR ',I5)
 1100 FORMAT ('UVDOUT: ',A4,' ERROR ',I5)
      END
