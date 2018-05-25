LOCAL INCLUDE 'NANS.INC'
C                                       Local include for NANS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2)
      REAL      XSIN, XDISIN, XBCNT, XNCNT, BLC(7), TRC(7),
     *   BUFF1(UVBFSS)
      INTEGER   SEQIN, DISKIN, CNOIN, BCOUNT, NCOUNT, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, LRECI, NRPRMI,
     *   IBUFF1(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, DATYPE*2
      EQUIVALENCE (IBUFF1, BUFF1)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XBCNT, XNCNT, BLC,
     *   TRC
      COMMON /OTPARM/ CATOLD, SEQIN, DISKIN, ILOCWT, INCSI, INCFI,
     *   INCIFI, LRECI, NRPRMI, ISCOMP, NCOUNT, BCOUNT, CNOIN
      COMMON /CHARPM/ NAMEIN, CLAIN, DATYPE
      COMMON /BUFRS/ BUFF1, JBUFSZ
C                                       End local include for NANS
LOCAL END
      PROGRAM NANS
C-----------------------------------------------------------------------
C! Looks for NaNs in a data set
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2015, 2017
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
C      NCOUNT         NCOUNT        Max number lines printed
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
      CALL NANSIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       UV data
      IF (DATYPE.EQ.'UV') THEN
         CALL NANSUV (IRET)
      ELSE
         CALL NANSIM (IRET)
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE NANSIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   NANSIN gets input parameters for NANS
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
      NPARM = 23
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
      NCOUNT = IROUND (XNCNT)
      IF (BCOUNT.LT.1) BCOUNT = 1
      IF (NCOUNT.LT.1) NCOUNT = 1000
C                                       Get CATBLK from old file.
      OLDCNO = 1
      DATYPE = '  '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   DATYPE, NLUSER, STAT, BUFF1, IERR)
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
      CNOIN = OLDCNO
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       UV
      IF (DATYPE.EQ.'UV') THEN
C                                       Compressed data?
         ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
         IF (ISCOMP) THEN
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *         ILOCWT, JERR)
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
C                                       Image
      ELSE IF (DATYPE.EQ.'MA') THEN
         CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
         MSGTXT = 'UNRECOGNIZED FILE TYPE = ''' // DATYPE // ''''
         JERR = 10
         GO TO 990
         END IF
C                                        Put input file in READ
      PTYPE = DATYPE
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
 1000 FORMAT ('NANSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE NANSUV (IRET)
C-----------------------------------------------------------------------
C   NANSUV sends uv data one point at a time to the checking routine
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
                  IF (XCOUNT.LT.NCOUNT) CALL MSGWRT (5)
                  XCOUNT = XCOUNT + 1
                  END IF
               IF (JRET.GT.0) THEN
                  WRITE (MSGTXT,1111) JRET, NUMVIS, IA1, IA2, JTT, RTT
                  IF (MSGTXT(56:56).EQ.' ') MSGTXT(56:56) = '0'
                  IF (YCOUNT.LT.NCOUNT) CALL MSGWRT (5)
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
 1000 FORMAT ('NANSUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('NANSUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('NANSUV: ERROR',I3,' READING VIS FILE')
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
      SUBROUTINE NANSIM (IRET)
C-----------------------------------------------------------------------
C   NANSIM reads an image and reports on the NaNs present (magic blanks
C   are expected and are not reported).
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'NANS.INC'
      INTEGER   IX, IY, I3, I4, I5, I6, I7, I1L, I2L, I3L, I4L, I5L,
     *   I6L, I7L, I1U, I2U, I3U, I4U, I5U, I6U, I7U, LUN, IND, BIND,
     *   WIN(4), NXI, NYI, DEPTH(5), BO, ICOUNT, NNAN, NX, NP, I, IP1,
     *   IP2, LIST(700), TCOUNT, XCOUNT
      CHARACTER PHNAME*48
      LOGICAL   T, F, FIRST
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (DEPTH(1), I3)
      EQUIVALENCE (DEPTH(2), I4)
      EQUIVALENCE (DEPTH(3), I5)
      EQUIVALENCE (DEPTH(4), I6)
      EQUIVALENCE (DEPTH(5), I7)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       set limits
      I1L = BLC(1) + 0.1
      I2L = BLC(2) + 0.1
      I3L = BLC(3) + 0.1
      I4L = BLC(5) + 0.1
      I5L = BLC(5) + 0.1
      I6L = BLC(6) + 0.1
      I7L = BLC(7) + 0.1
      I1U = TRC(1) + 0.1
      I2U = TRC(2) + 0.1
      I3U = TRC(3) + 0.1
      I4U = TRC(5) + 0.1
      I5U = TRC(5) + 0.1
      I6U = TRC(6) + 0.1
      I7U = TRC(7) + 0.1
      FIRST = .TRUE.
C                                       open image
      CALL ZPHFIL ('MA', DISKIN, CNOIN, 1, PHNAME, IRET)
      LUN = 16
      CALL ZOPEN (LUN, IND, DISKIN, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING IMAGE FILE'
         GO TO 990
         END IF
C                                       init IO
      WIN(1) = I1L
      WIN(2) = I2L
      WIN(3) = I1U
      WIN(4) = I2U
      NXI = CATBLK(KINAX)
      NYI = CATBLK(KINAX+1)
      NX = I1U - I1L + 1
      ICOUNT = 0
      XCOUNT = 0
      MSGTXT = ' Row Plane         Information'
      DO 70 I7 = I7L,I7U
         DO 60 I6 = I6L,I6U
            DO 50 I5 = I5L,I5U
               DO 40 I4 = I4L,I4U
                  DO 30 I3 = I3L,I3U
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH,
     *                  BO, IRET)
                     BO = BO + 1
                     CALL MINIT ('READ', LUN, IND, NXI, NYI, WIN, BUFF1,
     *                  JBUFSZ, BO, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT IMAGE I/O'
                        GO TO 990
                        END IF
                     TCOUNT = 0
                     DO 20 IY = I2L,I2U
                        CALL MDISK ('READ', LUN, IND, BUFF1, BIND, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'READ IMAGE ROW'
                           GO TO 990
                           END IF
                        CALL CNTNAN (NX, IBUFF1(BIND), NNAN)
                        XCOUNT = XCOUNT + NNAN
C                                       summarize plane
                        TCOUNT = TCOUNT + NNAN
                        IF (ICOUNT.LT.2*NCOUNT) THEN
C                                       full row?
                           IF (NNAN.GE.NX) THEN
                              IF (FIRST) CALL MSGWRT (5)
                              FIRST = .FALSE.
                              WRITE (MSGTXT,1100) IY, DEPTH
                              CALL MSGWRT (5)
                              ICOUNT = ICOUNT + 1
C                                       summarize row
                           ELSE IF ((NNAN.GT.0) .AND.
     *                        (ICOUNT.GE.NCOUNT)) THEN
                              IF (FIRST) CALL MSGWRT (5)
                              FIRST = .FALSE.
                              WRITE (MSGTXT,1101) IY, DEPTH, NNAN
                              CALL MSGWRT (5)
                              ICOUNT = ICOUNT + 1
C                                       report details
                           ELSE IF (NNAN.GT.0) THEN
                              NP = 0
                              DO 10 IX = 1,NX
                                 CALL CNTNAN (1, IBUFF1(BIND+IX-1), I)
                                 IF ((I.GT.0) .AND. (NP.LT.700)) THEN
                                    NP = NP + 1
                                    LIST(NP) = I1L + IX - 1
                                    END IF
 10                              CONTINUE
                              IP1 = 1
                              IF (FIRST) CALL MSGWRT (5)
                              FIRST = .FALSE.
 11                           IF (IP1.LE.NP) THEN
                                 IP2 = MIN (IP1+6, NP)
                                 WRITE (MSGTXT,1102) IY, DEPTH,
     *                              (LIST(I), I = IP1,IP2)
                                 IF (IP1.NE.1) MSGTXT(1:17) = ' '
                                 CALL MSGWRT (5)
                                 ICOUNT = ICOUNT + 1
                                 IP1 = IP2 + 1
                                 GO TO 11
                                 END IF
                              END IF
                           END IF
 20                     CONTINUE
C                                       plane summary if needed
                     IF (TCOUNT.GT.0) THEN
                        IF (FIRST) CALL MSGWRT (5)
                        FIRST = .FALSE.
                        WRITE (MSGTXT,1120) DEPTH, TCOUNT
                        CALL MSGWRT (5)
                        ICOUNT = ICOUNT + 1
                        END IF
 30                  CONTINUE
 40               CONTINUE
 50            CONTINUE
 60         CONTINUE
 70      CONTINUE
      CALL ZCLOSE (LUN, IND, I)
      WRITE (MSGTXT,1170) XCOUNT, ICOUNT
      CALL MSGWRT (5)
      GO TO 999
C
 990  CALL MSGWRT (8)
c
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NANSIM: ERROR',I4,' ON ',A)
 1100 FORMAT (I5,I4,4I2,'  Full row')
 1101 FORMAT (I5,I4,4I2,'  Number NaNs=',I5)
 1102 FORMAT (I5,I4,4I2,'  IX=',7I6)
 1120 FORMAT (5X,I4,4I2,'  Number NaNs in plane=',I8)
 1170 FORMAT ('Found',I9,' NaNs reported in',I4,' message lines')
      END
