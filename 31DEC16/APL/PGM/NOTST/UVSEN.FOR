LOCAL INCLUDE 'UVSEN.INC'
C                                       Local include for UVSEN
      INTEGER   CATBLK(256)
      HOLLERITH XNAME(3), XCLASS(2)
      CHARACTER NAME*12, CLASS*6
      REAL      XSEQ, XDISK, APARM(10)
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, APARM
      COMMON /CHRCOM/ NAME, CLASS
LOCAL END
      PROGRAM UVSEN
C-----------------------------------------------------------------------
C! Determine RMS sidelobe levels and sensitivity
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C   AIPS task UVSEN
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumne on which file resides
C      APARM          APARM         1,2,3 Gaussian start, end,
C                                         increment (arcsec)
C                                   Noise variance (Jy)
C   Programmer: T. Cornwell August 1984
C   An attempt has been made by Eric to make this more standard and to
C   get it to work on the real 15Oct84 system.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER PRGM*6, UTYPE*2
      LOGICAL   RQUICK, T, F
      INTEGER   DISK, SEQ, USERID, FIND, BIND, CNO, BUFSZ, LENBU, IRET,
     *   IERR, NGAUS, NIO, I, J, IPOINT, LUN, NPARM
      INTEGER   BO, PZERO
      REAL      BUFF(UVBFSS), ALPHA, AS2RD, U, V, WT, RR, RMAX, THETA,
     *   GAUS, FWHM, SIDEL, SWTG(256), SWT2G2(256), SWTG2(256), SWT,
     *   SWT2, SG(256), SG2(256), CATR(256), TEMPK
      DOUBLE PRECISION CATD(128), KPJY
      INCLUDE 'UVSEN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGM /'UVSEN '/
      DATA LUN, BO ,PZERO /16,1,0/
      DATA NPARM /18/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (T, BUFF)
      CALL VHDRIN
      LENBU = 15
C                                       Get input parameters.
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
C                                       Restart AIPS.
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       Decode input.
      USERID = NLUSER
      SEQ = XSEQ + 0.1
      DISK = XDISK + 0.1
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
C                                       Other parameters
      APARM(1) = MAX (APARM(1), 0.)
      APARM(2) = MAX (APARM(2), APARM(1))
      IF (APARM(3).GT.0.) NGAUS = (APARM(2) - APARM(1)) / APARM(3) + 1.
      IF (APARM(3).LE.0.) NGAUS = 1
      IF (NGAUS.LE.256) GO TO 15
          WRITE (MSGTXT,1010)
          CALL MSGWRT (8)
          IERR = 1
          GO TO 990
C                                       Initialize.
 15   IF (APARM(4).LE.0) APARM(4) = 0.02
      ALPHA =   (3.141592653579893)**2 / (4. * LOG(2.))
      AS2RD = 3.141592653579893 / 3600. / 180.
      SWT = 0.
      SWT2 = 0.
      DO 20 I = 1,NGAUS
         SWTG(I) = 0.
         SWT2G2(I) = 0.
         SWTG2(I) = 0.
 20      CONTINUE
C                                       Open file and get CATBLK.
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISK, NAME, CLASS, SEQ, UTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, BUFF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 990
C                                       Get info from CATBLK.
 30   CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 920
C                                       Disallow packed uv data
      IF (CATBLK(KINAX).EQ.1) THEN
         IERR = 9
         MSGTXT = 'ERROR: I CANNOT PROCESS PACKED UV DATA, USE SPLIT'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      LENBU = 0
      FREQ = CATD(KDCRV+2)
C                                       Initialize reading VIS. file.
      BUFSZ = UVBFSS  * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, PZERO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 920
C                                       Start looping thru data.
 50   CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 60
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (8)
            IRET = 8
            GO TO 100
 60      IPOINT = BIND
         IF (NIO.LE.0) GO TO 100
         DO 80 J = 1,NIO
            WT = BUFF(IPOINT+NRPARM+2)
C                                       Process an ok RR visibility.
            IF (WT.LE.0) GO TO 75
               U = BUFF(IPOINT+ILOCU)
               V = BUFF(IPOINT+ILOCV)
               RR = U * U + V * V
               RMAX = MAX(RMAX,RR)
               SWT = SWT + WT
               SWT2 = SWT2 + WT**2
               DO 70 I = 1,NGAUS
                  THETA = (APARM(1) + (I-1)*APARM(3)) * AS2RD
                  GAUS = RR * ALPHA * THETA**2
C                                       If exponent>20, assume zero.
                  IF (GAUS.GT.20.) GO TO 70
                     GAUS = EXP (-GAUS)
                     SWTG(I) = SWTG(I) + WT*GAUS
                     SWT2G2(I) = SWT2G2(I) + (WT*GAUS)**2
                     SWTG2(I) = SWTG2(I) + WT*GAUS**2
                     SG(I) = SG(I) + GAUS
                     SG2(I) = SG2(I) + GAUS**2
 70               CONTINUE
C                                       Update IPOINT
 75         IPOINT = IPOINT + LREC
 80         CONTINUE
         GO TO 50
C                                       User output
 100  SWT = SWT + SWT
      RMAX = SQRT(RMAX)
C                                       FWHM of jinc function
C                                       is 2*0.7050849
      FWHM = 2. * 0.7050849 / (2.*RMAX) / AS2RD
      KPJY = 1.224E24/FREQ**2
      WRITE (MSGTXT,1100) SWT
      CALL MSGWRT (5)
      WRITE (MSGTXT,1101) RMAX, FWHM
      CALL MSGWRT (5)
      WRITE (MSGTXT,1102)
      CALL MSGWRT (5)
      CALL OUTPUT (0., SQRT(2.*SWT2/SWT**2), 0., 0.)
      DO 110 I = 1,NGAUS
         THETA = APARM(1) + (I-1)*APARM(3)
         IF (THETA.LE.0) GO TO 110
            SIDEL = SQRT( SWT2G2(I)/(2.*SWTG(I)**2) )
C                                               1.1331=PI/(4*LOG(2))
C            SENS = APARM(4) * SQRT (1. / (2.*SWTG2(I))) /
C     *         (1.1331*THETA**2)
            WT =   APARM(4) * SQRT (SWTG2(I) / (2.*SWTG(I)**2)) /
     *         (1.1331*THETA**2)
            TEMPK = WT*KPJY
            CALL OUTPUT (THETA, SIDEL, WT, TEMPK)
 110     CONTINUE
      IRET = 0
C
 920  CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.EQ.0) GO TO 990
         WRITE (MSGTXT,1920) IERR
         CALL MSGWRT (6)
C                                        Write end message
 990  CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1010 FORMAT ('NUMBER OF GAUSSIANS EXCEEDS 256')
 1020 FORMAT ('ERROR:',I7,' OPENING UV FILE')
 1040 FORMAT ('ERROR:',I7,' INITILIZING UV FILE')
 1050 FORMAT ('ERROR:',I7,' READING VIS ')
 1100 FORMAT ('Sum of weights is ',F10.0)
 1101 FORMAT ('Max baseline is ',1PE11.3,' lambda, giving FWHM of',
     *   1PE11.3,' asec')
 1102 FORMAT ('  theta(asec)  sidelobe   Jy/(asec**2)  Temp K')
 1920 FORMAT ('ERROR:',I7,' CLOSING UV FILE ')
      END
      SUBROUTINE OUTPUT (THETA, SIDEL, WTS, T)
C-----------------------------------------------------------------------
C   Routine to output sensitivity and sidelobe size to the user.
C-----------------------------------------------------------------------
      REAL      THETA, SIDEL, WTS, T
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      WRITE (MSGTXT,1000) THETA, SIDEL, WTS, T
      CALL MSGWRT (5)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (F12.2,3(1X,1PE11.3))
      END
