LOCAL INCLUDE 'NANS.INC'
C                                       Local include for NANS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2)
      REAL      XSIN, XDISIN, XBCNT, XECNT, BUFF1(UVBFSS)
      INTEGER   SEQIN, DISKIN, BCOUNT, ECOUNT, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, LRECI, NRPRMI,
     *   IBUFF1(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6
      EQUIVALENCE (IBUFF1, BUFF1)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XBCNT, XECNT
      COMMON /OTPARM/ CATOLD, SEQIN, DISKIN, ILOCWT, INCSI, INCFI,
     *   INCIFI, LRECI, NRPRMI, ISCOMP, ECOUNT, BCOUNT
      COMMON /CHARPM/ NAMEIN, CLAIN
      COMMON /BUFRS/ BUFF1, JBUFSZ
C                                       End local include for NANS
LOCAL END
      PROGRAM NANS
C-----------------------------------------------------------------------
C! Looks for NaNs in a data set
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2015
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
C   NANS finds NaNs in a UV data set
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      BCOUNT         BCOUNT        Start vis in UV data set
C      ECOUNT         ECOUNT        Max number lines printed
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'NANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'NANS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FUDGIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL SENDUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FUDGIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   FUDGIN gets input parameters for NANS
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in NANS for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'NANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 9
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      BCOUNT = IROUND (XBCNT)
      ECOUNT = IROUND (XECNT)
      IF (BCOUNT.LT.1) BCOUNT = 1
      IF (ECOUNT.LT.1) ECOUNT = 1000
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FUDGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE SENDUV (IRET)
C-----------------------------------------------------------------------
C   SENDUV sends uv data one point at a time to the checking routine
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   INIO, IPTRI, LUNI, INDI, ILENBU, IBIND, I, IA1, IA2,
     *   BO, VO, NUMVIS, XCOUNT, NCOPY, JTT(3), JRET, YCOUNT
      LOGICAL   T, F
      INCLUDE 'NANS.INC'
      REAL      BASEN, RTT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCOPY = LRECI
      IF (ISCOMP) THEN
         NCOPY = NRPRMI
         MSGTXT = 'CHECKS RANDOM PARAMETERS ONLY FOR COMPRESSED DATA'
         CALL MSGWRT (7)
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      VO = BCOUNT - 1
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = BCOUNT - 1
      XCOUNT = 0
      YCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Loop over buffer
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS,100000).EQ.0) THEN
               WRITE (MSGTXT,1105) NUMVIS
               CALL MSGWRT (2)
               END IF
C                                       Call user routine.
            CALL CNTNAN (NCOPY, IBUFF1(IPTRI), IRET)
            CALL CNTIND (NCOPY, BUFF1(IPTRI), JRET)
C                                       tell of NaN(s)
            IF ((IRET.GT.0) .OR. (JRET.GT.0)) THEN
               IF (ILOCB.GE.0) THEN
                  BASEN = BUFF1(IPTRI+ILOCB)
                  IA1 = BASEN / 256. + 0.1
                  IA2 = BASEN - IA1*256. + 0.1
               ELSE
                  IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
                  IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
                  END IF
               CALL T2DHMS (BUFF1(IPTRI+ILOCT), 3, JTT, RTT)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1110) IRET, NUMVIS, IA1, IA2, JTT, RTT
                  IF (MSGTXT(56:56).EQ.' ') MSGTXT(56:56) = '0'
                  IF (XCOUNT.LT.ECOUNT) CALL MSGWRT (5)
                  XCOUNT = XCOUNT + 1
                  END IF
               IF (JRET.GT.0) THEN
                  WRITE (MSGTXT,1111) JRET, NUMVIS, IA1, IA2, JTT, RTT
                  IF (MSGTXT(56:56).EQ.' ') MSGTXT(56:56) = '0'
                  IF (YCOUNT.LT.ECOUNT) CALL MSGWRT (5)
                  YCOUNT = YCOUNT + 1
                  END IF
               END IF
            IPTRI = IPTRI + LRECI
 190        CONTINUE
         IF (INIO.GT.0) GO TO 100
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      WRITE (MSGTXT,1200) XCOUNT
      CALL MSGWRT (5)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SENDUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('SENDUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SENDUV: ERROR',I3,' READING VIS FILE')
 1110 FORMAT (I7,' NaNs at vis',I10,' ants, T',I3.2,' -',I2.2,I4,'/',
     *   2(I2.2,':'),F6.3)
 1111 FORMAT (I7,' INDEs at vis',I10,' ants, T',I3.2,' -',I2.2,I4,'/',
     *   2(I2.2,':'),F6.3)
 1105 FORMAT ('At vis number',I12)
 1200 FORMAT (I10,' vis records had NaNs')
      END
      SUBROUTINE T2DHMS (TIMEIN, NDIG, TIME, RTIME)
C-----------------------------------------------------------------------
C   Convert from Time to Days Hours Minutes Seconds format
C   Input:
C      TIMEIN   R       Input:  Time
C      NDIG     I       Number digits in seconds display
C   Output:
C      TIME     I*(3)   Output Time in Days Hours Minutes
C      RTIME    R       SECONDS
C-----------------------------------------------------------------------
      REAL     TIMEIN, RTIME
      INTEGER  NDIG, TIME(3)
C
      REAL     T
      INTEGER  I, J
C-----------------------------------------------------------------------
      T = TIMEIN
      IF (TIMEIN.LT.0.0) T = -T
C
      TIME(1) = T
      T = (T - TIME(1)) * 24.0
      TIME(2) = T
      T = (T - TIME(2)) * 60.0
      TIME(3) = T
      T = (T - TIME(3)) * 60.0
      RTIME   = T
      J = 10 ** NDIG
      J = MAX (1, J)
      I = J*T + 0.5
C                                       Now Remove 60 seconds
      IF (I.GE.J*60) THEN
         RTIME = RTIME - 60.0
         TIME(3) = TIME(3) + 1
         END IF
C                                       Now Remove 60 minutes
      IF (TIME(3).GE.60) THEN
         TIME(3) = TIME(3) - 60
         TIME(2) = TIME(2) + 1
         END IF
C                                       Now Remove 24 hours
      IF (TIME(2).GE.24) THEN
         TIME(2) = TIME(2) - 24
         TIME(1) = TIME(1) + 1
         END IF
C                                       Sign
      IF (TIMEIN.LT.0.0) TIME(1) = -TIME(1)
C
 999  RETURN
      END
      SUBROUTINE CNTNAN (N, IARR, CNT)
C-----------------------------------------------------------------------
C   count NaNs
C   Inputs:
C      N      I      Number values to check
C      IARR   I(*)   Values to check
C   Outputs:
C      CNT    I      Number of NaNs
C-----------------------------------------------------------------------
      INTEGER   N, IARR(*), CNT
C
      INTEGER   I, I4NAN, J, ZAND
C                                       = 7F800000 mask for exponent
      DATA I4NAN /2139095040/
C-----------------------------------------------------------------------
      CNT = 0
      DO 10 I = 1,N
         J = ZAND (IARR(I), I4NAN)
         IF (J.EQ.I4NAN) CNT = CNT + 1
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CNTIND (N, IARR, CNT)
C-----------------------------------------------------------------------
C   count INDEs
C   Inputs:
C      N      I      Number values to check
C      IARR   I(*)   Values to check
C   Outputs:
C      CNT    I      Number of NaNs
C-----------------------------------------------------------------------
      INTEGER   N, CNT
      REAL      IARR(*)
C
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CNT = 0
      DO 10 I = 1,N
         IF (IARR(I).EQ.FBLANK) CNT = CNT + 1
 10      CONTINUE
C
 999  RETURN
      END
