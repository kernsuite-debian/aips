      SUBROUTINE GRIDTB (DPARM, IMSIZE, NCHAVG, CHINC, NXANT, NXBASL,
     *   IXANT, IXBASL, DESEL, MSOU, MTIMES, MAXBUF, DIMBUF, IOBUF,
     *   XDOCAT, OUTNAM, OUTCLS, SEQOUT, DISKOU, CNOOUT, NPOINT, NFAIL,
     *   INSNUM, NANTSK, NBUFF, SBUFF, RBUFF, IRET)
C-----------------------------------------------------------------------
C! Makes a gridded image of the UV data in TB order.
C# UV Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2008-2011, 2013-2017
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
C   GRIDTB makes a gridded image of the UV data in TB order.
C   Inputs:
C      DPARM    R(10)   Control parms:
C                          (1) 0,1,2 => amp, phase, amp rms gridded
C                              3 amp rms/mean gridded
C                              4, 5 => real, imag part of vis
C                              6 => store real and imag for TVFLG
C                          (2) > scalar avg, else vector
C                          (3) > 0 B -> ant. pair (grid twice)
C                          (4) > 0 => divide amp by source flux
C                          (5) x-axis interval
C                          (6) y-axis interval
C      IMSIZE   I(2)    Output image size (x,y)
C      NCHAVG   I       Number channels to average together
C      CHINC    I       Channel increment
C      NXANT    I       # entries in IXANT
C      NXBASL   I       # entries in IXBASL
C      IXANT    I(50)   ANTENNAS adverb values
C      IXBASL   I(50)   BASELINE adverb values
C      DESEL    L       the Antennas/Baselines are deselected
C      MAXBUF   I       Number of buffers to use
C      DIMBUF   I       Size of summing 1 buffer
C      IOBUF    I       Size of 1 IO buffer
C      XDOCAT   R       > 0 => mark the new image as permanent in the
C                       DFIL commons, else as temporary
C   In/out:
C      MSOU     I(*)    source numbers corresponding to time intervals
C                          made = -1 if times set regular
C      MTIMES   R(*)    (* = IMSIZE(2)) start times for each row
C                          MTIMES(2) < MTIMES(1) < 0 => set regular
C                          times from the first datum
C      OUTNAM   C*12    Output image name
C      OUTCLS   C*6     Output image class
C      SEQOUT   I       Output image seq number, if scratch, is DFIL
C                       number of SC file
C      DISKOU   I       Output image disk
C   Output:
C      CNOOUT   I       Output image catalog number
C      NPOINT   D       Vis included in grid  - note double precision!
C      NFAIL    I       Vis included but off the grid
C      NANTSK   I       Vis dropped due to antennas/baseline
C      NBUFF    I(*)    Counting buffer (DIMBUF,MAXBUF)
C      SBUFF    R(*)    Summing buffer (DIMBUF,MAXBUF)
C      RBUFF    R(*)    IO buffer (IOBUF,MAXBUF)
C      IRET     I       Error code: 0 ok, else die
C   Expected:
C      MAXBUF = 8       max. no buffers
C      DIMBUF = 2 * MAXANT * MAXANT     max baselines
C      IOBUF  = 16384   Words of IO buffer
C-----------------------------------------------------------------------
      INTEGER   IMSIZE(2), NCHAVG, CHINC, NXANT, NXBASL, IXANT(50),
     *   IXBASL(50), MSOU(*), MAXBUF, DIMBUF, IOBUF, SEQOUT, DISKOU,
     *   CNOOUT, NFAIL, INSNUM, NANTSK, NBUFF(DIMBUF,MAXBUF), IRET
      DOUBLE PRECISION NPOINT
      REAL      DPARM(10), XDOCAT, MTIMES(*), SBUFF(DIMBUF,MAXBUF),
     *   RBUFF(IOBUF,MAXBUF)
      LOGICAL   DESEL
      CHARACTER OUTNAM*12, OUTCLS*6
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER PHNAME*48, AXTYPE(3)*8, CUNITS(2)*8, CHTEMP*2, TYPTMP*2,
     *   CHSTOK(23)*4
      HOLLERITH CATH(256), CATSH(256), CATIH(256)
      INTEGER   LTYPE(2), I, J, K, IERR, IA, JERR, CATSAV(256), LUNO,
     *   LIF, LCHAN, IBLKOF, NBYT, IDEPTH(5), LBIF, LEIF, LBCHAN, NCOL,
     *   LECHAN, NROW, IROW, ITYP, OLDSOU, IROUND, NUMAN(1025), ITEMP,
     *   CATIMG(256), IANT, JANT, IARR, COLMUL, IJ, KP, KCH, KIF,
     *   NCOL2, LNCFIL, COLUP, LSUNUM, LLSUN, LLARR, LBVIS, LSTSOU, II,
     *   LIMIT, COLCRV, NSTOK, IB, IV, NAXBUF, NUMBUF, MXVIS, NOVIS,
     *   LCHINC, LIFINC, IBUFF, LR, IVIS, LSTINC, IVISO, MROW, INCB,
     *   LNCF, LNCS, LNCIF, IIV, NCHAN, ISOFF(4)
      LOGICAL   T, F, DOTWO, ISINGL, TABLE, EXIST, FITASC, MULTIS,
     *   WASSOU, GOTVIS, ISEOF, FIRST, REQBAS
      REAL      CATR(256), CATSR(256),  FLUXX, VALUE1, VALUE2, VALUE3,
     *   CATIR(256), TIMEND, RMAX, RMIN, TIMLST, RPARM(20), TEMP, WT
      DOUBLE PRECISION    CATD(128), CATSD(128), CATID(128), TSIGMA
C                                       MXVIS = max. no vis in VBUFF
      PARAMETER (MXVIS=100)
      REAL      VBUFF(30*MXVIS), DAFLUX(8), VISSAV(24), DBUFF(UVBFSS)
      INTEGER   MUMBUF, LUN(8), FIND(8), ICOL1(MXVIS), ICOL2(MXVIS),
     *   IBPTR(8), NCOLMX
      SAVE DBUFF
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      COMMON /UVIMGC/ CATIMG
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),
     *   (CATSAV, CATSH, CATSR, CATSD), (CATIMG, CATIH, CATIR, CATID)
      DATA AXTYPE /'ANT PAIR', 'BASELINE', 'TIME    '/
      DATA LUNO/26/
      DATA LUN /16, 17, 18, 19, 20, 21, 22, 23/
      DATA T, F /.TRUE.,.FALSE./
      DATA CUNITS /'DEGREES ','RATIO   '/
      DATA CHSTOK /'I','Q','U','V','IQU','IQUV','IV','QU', 'RR','LL',
     *   'RL','LR','RRLL','RLLR','XX','YY','XY','YX','XXYY','XYYX',
     *   'HALF','FULL','CROS'/
C-----------------------------------------------------------------------
      IRET = 0
C                                        NCOLMX = 2 * MAXANT * MAXANT
      NCOLMX = DIMBUF
      NAXBUF = MIN (8, MAXBUF)
      IF (MAXBUF.GT.8) THEN
         WRITE (MSGTXT,1000) MAXBUF
         CALL MSGWRT (6)
         END IF
      LSTSOU = -1
      WASSOU = .FALSE.
      FIRST = (MTIMES(1).LT.0.0) .AND. (MTIMES(2).LE.MTIMES(1))
      LLSUN = INSNUM
      LLARR = 0
      OLDSOU = 0
      NPOINT = 0.0D0
      NFAIL = 0
      NANTSK = 0
      DO 10 IBUFF = 1,NAXBUF
         DAFLUX(IBUFF) = 1.0
 10      CONTINUE
      ITYP = IROUND (DPARM(1)) + 1
      IF ((ITYP.EQ.1) .AND. (DPARM(2).GT.0.0)) ITYP = 0
C                                       ITYP = output data type:
C                                       0 = amp (scalar average)
C                                       1 = amp, 2 = phase, 3 = rms
C                                       4 = amp rms / mean
C                                       5 = real, 6 = imag,
C                                       7 = real & imag & flag #
      IF (ITYP.EQ.7) THEN
         COLMUL = 3
         COLUP = 3
      ELSE
         COLMUL = 1
         COLUP = 0
         END IF
      LTYPE(1) = 1
      LTYPE(2) = 2
      CALL H2CHR (2, 1, CATBLK(KITYP), CHTEMP)
      IF (CHTEMP(:1).NE.'T') THEN
         WRITE (MSGTXT,1005) CHTEMP
         IRET = 2
         GO TO 990
         END IF
C                                       Open the UV file
      TYPTMP = 'UV'
      CALL MAPOPN ('READ', IUDISK, UNAME, UCLAS, IUSEQ, TYPTMP, NLUSER,
     *   LUN, FIND, IUCNO, CATBLK, VBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN INPUT UV', IRET
         IRET = 1
         GO TO 990
         END IF
C                                       and close (UVGET does open)
      CALL MAPCLS ('READ', IUDISK, IUCNO, LUN, FIND, CATBLK, F, VBUFF,
     *   IERR)
C                                       Do we want multi-source?
C                                       only for flux scaling here
      EXIST = .FALSE.
      IF (DPARM(4).GT.0.0) CALL ISTAB ('SU', IUDISK, IUCNO, 1, LUN,
     *   VBUFF, TABLE, EXIST, FITASC, IERR)
      ISINGL = (.NOT.EXIST) .OR. (IERR.NE.0)
      ISINGL = ISINGL .OR. ((ILOCSU.LT.0) .AND. (INSNUM.LE.0))
      IF (ISINGL) DPARM(4) = -1.0
      MULTIS = ILOCSU.GE.0
C                                       Get number antennas
      IF ((LTYPE(1).EQ.1) .AND. (DPARM(3).GT.0.0)) LTYPE(1) = 0
      DOTWO = LTYPE(1).EQ.0
      CALL GETNAN (IUDISK, IUCNO, CATBLK, LUN, VBUFF, NUMAN, IRET)
      IF (((IRET.NE.0) .AND. (IRET.NE.10)) .OR. (NUMAN(1).LE.0)) THEN
         WRITE (MSGTXT,1015) IRET, NUMAN(1)
         IRET = 4
         GO TO 990
         END IF
      J = 0
      LIF = NUMAN(1)
      DO 25 I = 1,LIF
         NUMAN(513+I) = J
         IF (DOTWO) THEN
            J = J + NUMAN(1+I) * (NUMAN(1+I) + 1)
         ELSE
            J = J + (NUMAN(1+I) * (NUMAN(1+I) + 3)) / 2
            END IF
 25      CONTINUE
      NUMAN(514+LIF) = J
C                                       Build image header:
C                                       Misc.
      CALL COPY (256, CATBLK, CATSAV)
      CALL UVPGET (IRET)
      IF (ITYP.EQ.2) CALL CHR2H (8, CUNITS(1), 1, CATH(KHBUN))
      IF (ITYP.EQ.4) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      CATBLK(KIGCN) = 1
      CATBLK(KIPCN) = 0
      CATR(KRBLK) = FBLANK
      CATBLK(KIIMU) = NLUSER
      CATBLK(KITYP) = 0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C                                       Coordinates
      CATBLK(KIDIM) = 7
C                                       FREQ
      NCHAN = (ECHAN - BCHAN) / CHINC + 1
      CATBLK(KINAX+3) = NCHAN
      CATD(KDCRV+3) = CATSD(KDCRV+JLOCF)
      CATR(KRCIC+3) = CATSR(KRCIC+JLOCF) * CHINC
      CATR(KRCRP+3) = (CATSR(KRCRP+JLOCF) - BCHAN - (NCHAVG-1)/2.0) /
     *   CHINC + 1
      CATR(KRCRT+3) = 0.0
      I = JLOCF * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+6))
C                                       IF
      CATR(KRCRT+4) = 0.0
      IF (JLOCIF.GE.0) THEN
         CATBLK(KINAX+4) = EIF - BIF + 1
         CATD(KDCRV+4) = CATSD(KDCRV+JLOCIF)
         CATR(KRCIC+4) = CATSR(KRCIC+JLOCIF)
         CATR(KRCRP+4) = CATSR(KRCRP+JLOCIF) - BIF + 1
         I = JLOCIF * 2
         CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+8))
      ELSE
         CATBLK(KINAX+4) = 1
         CATD(KDCRV+4) = 1.0D0
         CATR(KRCIC+4) = 1.0
         CATR(KRCRP+4) = 1.0
         CALL CHR2H (8, 'IF      ', 1, CATH(KHCTP+8))
         END IF
C                                       Stokes
      J = 0
      DO 35 I = 1,22
         IF (STOKES.EQ.CHSTOK(I)) J = I
 35      CONTINUE
      IF ((J.LE.0) .AND. (ICOR0.LT.-5)) J = 16
      IF ((J.LE.0) .AND. (ICOR0.LT.-4)) J = 19
      IF ((J.LE.0) .AND. (ICOR0.LT.-1)) J = 10
      IF ((J.LE.0) .AND. (ICOR0.LT.0)) J = 13
      IF (NCOR.EQ.1) THEN
         TEMP = CATSD(KDCRV+JLOCS) + (1.0-CATSR(KRCRP+JLOCS)) *
     *      CATSR(KRCIC+JLOCS)
         IF (TEMP.LT.-4.) THEN
            J = 10 - IROUND (TEMP)
         ELSE IF (TEMP.LT.0.) THEN
            J = 8 - IROUND (TEMP)
         ELSE
            J = IROUND (TEMP)
            END IF
      ELSE IF ((NCOR.EQ.2) .AND. (J.EQ.22)) THEN
         IF (ICOR0.LT.-4) THEN
            J = 19
            IF (ICOR0.LT.-6) J = 20
         ELSE IF (ICOR0.LT.0) THEN
            J = 13
            IF (ICOR0.LT.-2) J = 14
         ELSE
            J = 7
            END IF
         END IF
      IF (J.LE.0) J = 1
      IF (ICOR0.LT.-4) THEN
         IF (J.EQ.23) J = 20
         IF (J.EQ.21) J = 19
      ELSE
         IF (J.EQ.23) J = 14
         IF (J.EQ.21) J = 13
         END IF
      ISOFF(1) = 0
      ISOFF(2) = 1
      ISOFF(3) = 2
      ISOFF(4) = 3
      IF (J.LE.4) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = J
         CATR(KRCIC+2) = 1.0
         ISOFF(2) = -1
      ELSE IF (J.EQ.5) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 3
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 1.0
      ELSE IF (J.EQ.6) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 4
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 1.0
      ELSE IF (J.EQ.7) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 3.0
      ELSE IF (J.EQ.8) THEN
         STOKES = CHSTOK(5)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 2.0D0
         CATR(KRCIC+2) = 1.0
         ISOFF(1) = 1
         ISOFF(2) = 2
      ELSE IF (J.LE.12) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 8.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF(2) = -1
      ELSE IF (J.EQ.13) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -1.0D0
         CATR(KRCIC+2) = -1.0
      ELSE IF (J.EQ.14) THEN
         STOKES = CHSTOK(14)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -3.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF(1) = 2
         ISOFF(2) = 3
      ELSE IF (J.LE.18) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 10.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF(2) = -1
      ELSE IF (J.EQ.19) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -5.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF(2) = 1
      ELSE IF (J.EQ.20) THEN
         STOKES = CHSTOK(20)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -7.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF(1) = 2
         ISOFF(2) = 3
      ELSE IF (J.EQ.22) THEN
         STOKES = CHSTOK(22)
         CATBLK(KINAX+2) = 4
         CATD(KDCRV+2) = ICOR0
         CATR(KRCIC+2) = -1.0
         END IF
      NSTOK = CATBLK(KINAX+2)
      CATR(KRCRP+2) = 1.0
      CATR(KRCRT+2) = 0.0
      I = JLOCS * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+4))
C                                       warning message
      IF ((.NOT.DOCAL) .AND. (ICOR0.LE.0) .AND. (J.LE.8) .AND.
     *   (ILOCSU.GE.0)) THEN
         MSGTXT = 'WARNING: Conversion to Stokes before calibration'
     *      // ' is meaningless'
         CALL MSGWRT (2)
         END IF
C                                       RA, dec
      CATBLK(KINAX+5) = 1
      CATD(KDCRV+5) = CATSD(KDCRV+JLOCR)
      CATR(KRCIC+5) = CATSR(KRCIC+JLOCR)
      CATR(KRCRP+5) = CATSR(KRCRP+JLOCR)
      CATR(KRCRT+5) = CATSR(KRCRT+JLOCR)
      I = JLOCR * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+10))
      CATBLK(KINAX+6) = 1
      CATD(KDCRV+6) = CATSD(KDCRV+JLOCD)
      CATR(KRCIC+6) = CATSR(KRCIC+JLOCD)
      CATR(KRCRP+6) = CATSR(KRCRP+JLOCD)
      CATR(KRCRT+6) = CATSR(KRCRT+JLOCD)
      I = JLOCD * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+12))
C                                       First 2 axes
      DO 55 I = 1,2
         CATBLK(KINAX+I-1) = IMSIZE(I)
         IF (DPARM(4+I).EQ.0.0) DPARM(4+I) = 1.0
         CATR(KRCIC+I-1) = ABS (DPARM(4+I))
         IF (LTYPE(I).EQ.2) THEN
            CATR(KRCIC+I-1) = CATR(KRCIC+I-1) / (24. * 3600.)
            TSIGMA = CATR(KRCIC+I-1) / 11.0D0
            END IF
         CATD(KDCRV+I-1) = (MTIMES(1) + MTIMES(2)) / 2.0
         IF (LTYPE(I).LE.1) THEN
            CATR(KRCIC+I-1) = 1.0
            IF (SUBARR.GT.0) THEN
               CATD(KDCRV+I-1) = NUMAN(513+SUBARR) + 1.0D0
               CATBLK(KINAX+I-1) = NUMAN(514+SUBARR) - NUMAN(513+SUBARR)
            ELSE
               CATD(KDCRV+I-1) = 1.0D0
               CATBLK(KINAX+I-1) = NUMAN(514+NUMAN(1))
               END IF
            END IF
         CATR(KRCRP+I-1) = 1.0
         CATR(KRCRT+I-1) = 0.0
         J = (I-1) * 2
         CALL CHR2H (8, AXTYPE(LTYPE(I)+1), 1, CATH(KHCTP+J))
 55      CONTINUE
      IF (CATBLK(KINAX).GT.2*MAXANT*MAXANT) THEN
         IV = 2 * MAXANT * MAXANT
         WRITE (MSGTXT,1055) CATBLK(KINAX), IV
         CALL MSGWRT (8)
         MSGTXT = '        USE ONE SUBARRAY AT A TIME'
         IRET = 6
         GO TO 990
         END IF
      CATBLK(KINAX) = CATBLK(KINAX) * COLMUL + COLUP
      IMSIZE(1) = CATBLK(KINAX)
      IMSIZE(2) = CATBLK(KINAX+1)
      MROW = CATBLK(KINAX+1)
C                                       Create output map file
      LNCFIL = 0
C                                       Cataloged MA file output
      CALL MAKOUT (UNAME, UCLAS, IUSEQ, ' ', OUTNAM, OUTCLS, SEQOUT)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       support BADDISK
      IIV = 0
      IF (DISKOU.GT.0) THEN
         IV = DISKOU
         IIV = IV
         CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
      ELSE
         DO 65 IV = 1,NVOL
            DO 60 I = 1,10
               IF (IV.EQ.IBAD(I)) GO TO 65
 60            CONTINUE
            MSGSUP = 32000
            IIV = IV
            CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
            MSGSUP = 0
            IF (IRET.NE.1) GO TO 70
 65         CONTINUE
         IRET = 1
         END IF
 70   SEQOUT = CATBLK(KIIMS)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'CREATE OUTPUT IMAGE', IRET
         IF (IRET.EQ.1) THEN
            CALL MSGWRT (8)
            MSGTXT = 'NO DISK SPACE ON ALLOWED DISKS'
            IF (IIV.LE.0) MSGTXT = 'BADDISK LEAVES NO DISKS TO USE'
            END IF
         IRET = 3
         GO TO 990
         END IF
      DISKOU = IV
C                                       mark cataloged
      IF (XDOCAT.GT.0.0) THEN
         IF (NCFILE.LT.FILIST) THEN
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKOU
            FCNO(NCFILE) = CNOOUT
            FRW(NCFILE) = 2
            LNCFIL = NCFILE
         ELSE
            IRET = 9
            MSGTXT = 'GRIDTB: FILE LIST OVERFLOWS'
            END IF
C                                       mark as temporary
      ELSE
         IF (NSCR.LT.SCRLST) THEN
            NSCR = NSCR + 1
            SCRVOL(NSCR) = DISKOU
            SCRCNO(NSCR) = CNOOUT
         ELSE
            IRET = 9
            MSGTXT = 'GRIDTB: FILE LIST OVERFLOWS'
            END IF
         END IF
      IF (IRET.EQ.9) THEN
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IERR)
C                                       Loop over IF, freq
      IF (CATSAV(KINAX).EQ.1) THEN
         LNCS = INCS * 3
         LNCF = INCF * 3
         LNCIF = INCIF * 3
      ELSE
         LNCS = INCS
         LNCF = INCF
         LNCIF = INCIF
         ENDIF
      NBYT = 2 * IOBUF
      LBIF = BIF
      LEIF = EIF
      LBCHAN = BCHAN
      LECHAN = BCHAN + (NCHAN-1) * CHINC + NCHAVG - 1
      IF (LECHAN.NE.ECHAN) THEN
         WRITE (MSGTXT,1070) LECHAN, ECHAN
         CALL MSGWRT (6)
         END IF
      NCOL = CATBLK(KINAX)
      NCOL2 = (CATBLK(KINAX) - COLUP) / COLMUL
      CALL COPY (256, CATBLK, CATIMG)
      CALL COPY (256, CATSAV, CATBLK)
      RMAX = -1.E23
      RMIN = 1.E23
      LSTINC = CATIMG(KINAX+2)
      IB = NAXBUF / LSTINC

C                                       use buffers for several ch.
      LCHINC = NCHAN
      LCHINC = MAX (1, MIN (LCHINC, IB))
      LIFINC = 1
C                                       Use buffers for IFs.
      IF (LCHINC.LE.1) THEN
         LIFINC = LEIF - LBIF + 1
         LIFINC = MAX (1, MIN (LIFINC, IB))
         END IF
      NUMBUF = MAX (LCHINC, LIFINC) * LSTINC
      LBVIS = 3 * NUMBUF  + 4
      LIMIT = LBVIS - 4
      COLCRV = CATID(KDCRV) - 0.5001
      INCB = (LIFINC - 1) * LNCIF + (LCHINC - 1) * LNCF
      DO 500 LIF = LBIF,LEIF,LIFINC
         DO 480 LCHAN = 1,NCHAN,LCHINC
C                                       Init i/o to uv file
            IROW = 1
            BIF = LIF
            EIF = LIF + LIFINC - 1
            EIF = MIN (EIF, LEIF)
            BCHAN = (LCHAN-1) * CHINC + LBCHAN
            ECHAN = BCHAN + (LCHINC - 1) * CHINC + NCHAVG - 1
            ECHAN = MIN (ECHAN, LECHAN)
            MUMBUF = (ECHAN - BCHAN + CHINC - NCHAVG) / CHINC + 1
            MUMBUF = MUMBUF * (EIF - BIF + 1) * LSTINC
            IB = MUMBUF / LSTINC
            RPARM(1) = FBLANK
            TIMLST = -1.E10
            CALL UVGET ('INIT', RPARM, DBUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT INPUT UV', IRET
               IRET = 5
               GO TO 990
               END IF
            LNCS = INCS
            LNCF = INCF
            LNCIF = INCIF
C                                       init image I/O
            DO 80 IBUFF = 1,MUMBUF
C                                       Open output files on first pass
               IF ((LIF.EQ.LBIF) .AND. (LCHAN.EQ.1)) THEN
                  CALL ZOPEN (LUN(IBUFF), FIND(IBUFF), DISKOU, PHNAME,
     *               T, F, T, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) 'OPEN OUTPUT IMAGE', IRET
                     IRET = 4
                     GO TO 990
                     END IF
                  END IF
               IDEPTH(1) = MOD (IBUFF-1, LSTINC) + 1
               IDEPTH(2) = LCHAN
               IDEPTH(3) = LIF - LBIF + 1
               IDEPTH(4) = 1
               IDEPTH(5) = 1
C                                       Several channels
               IF (LCHINC.GT.1) THEN
                  IDEPTH(2) = IDEPTH(2) + (IBUFF-1) / LSTINC
C                                       Several IFs
               ELSE
                  IDEPTH(3) = IDEPTH(3) + (IBUFF-1) / LSTINC
                  END IF
               CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH,
     *            IBLKOF, IERR)
               IBLKOF = IBLKOF + 1
               CALL MINIT ('WRIT', LUN(IBUFF), FIND(IBUFF),
     *            CATIMG(KINAX), CATIMG(KINAX+1), 0, RBUFF(1,IBUFF),
     *            NBYT, IBLKOF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'INIT OUTPUT IMAGE', IRET
                  IRET = 4
                  GO TO 990
                  END IF
 80            CONTINUE
C                                       current row = 0
            NROW = 0
            IVIS = 1
            GOTVIS = F
            TIMEND = -1.0E20
C                                       Read first record
 90         IF (.NOT.GOTVIS) THEN
               IRET = 0
               CALL DATGET (RPARM, DBUFF, TIMLST, IRET)
               ISEOF = (IRET.EQ.4)
               IF (ISEOF) GO TO 200
               IF (IRET.NE.0) THEN
                  IF (IRET.EQ.4) THEN
                     MSGTXT = 'NO DATA SELECTED, CHECK ADVERBS'
                  ELSE
                     WRITE (MSGTXT,1010) 'READ INPUT UV', IRET
                     END IF
                  IRET = 5
                  GO TO 990
               ELSE
                  II = IVIS
                  DO 94 KIF = BIF,EIF
                     DO 93 J = 1,LCHINC
                        KCH = BCHAN + (J-1)*CHINC
                        DO 92 KP = 1,NSTOK
                           II = II + 3
                           VBUFF(II+1) = 0.0
                           VBUFF(II+2) = 0.0
                           VBUFF(II+3) = 0.0
                           DO 91 K = KCH,KCH+NCHAVG-1
                              IJ = LNCS * ISOFF(KP) + LNCF * (K-BCHAN)
     *                           + LNCIF * (KIF-BIF)
                              WT = DBUFF(IJ+3)
                              IF (WT.GT.0.0) THEN
                                 VBUFF(II+1) = VBUFF(II+1) + DBUFF(IJ+1)
     *                              * WT
                                 VBUFF(II+2) = VBUFF(II+2) + DBUFF(IJ+2)
     *                              * WT
                                 VBUFF(II+3) = VBUFF(II+3) + WT
                                 END IF
 91                           CONTINUE
                           WT = VBUFF(II+3)
                           IF (WT.GT.0.0) THEN
                              VBUFF(II+1) = VBUFF(II+1) / WT
                              VBUFF(II+2) = VBUFF(II+2) / WT
                              END IF
 92                        CONTINUE
 93                     CONTINUE
 94                  CONTINUE
                  END IF
               END IF
            GOTVIS = T
C                                       set regular times by default
            IF (FIRST) THEN
               II = RPARM(1+ILOCT) / TSIGMA + 0.5D0
               TEMP = II * TSIGMA
               CATID(KDCRV+1) = TEMP
               II = CATIMG(KINAX+1) + 1
               DO 97 I = 1,II
                  MSOU(I) = -1
                  MTIMES(I) = TEMP + (I - 1.5) * CATIR(KRCIC+1)
 97            CONTINUE
               FIRST = .FALSE.
               END IF
C                                       find row for integration
            IF (TIMEND.LT.-1.0E10) THEN
               TEMP = RPARM(1+ILOCT)
               IF ((IROW.LT.1) .OR. (IROW.GT.MROW)) IROW = 1
               DO 95 LR = IROW,MROW
                  IF ((TEMP.GE.MTIMES(LR)) .AND.
     *               (TEMP.LT.MTIMES(LR+1))) GO TO 96
 95               CONTINUE
               NFAIL = NFAIL + MUMBUF
               GOTVIS = F
               GO TO 90
 96            IROW = LR
               TIMEND = MTIMES(IROW+1)
C                                       New row, allow new source
               IF (IROW.GT.NROW) THEN
                  LLSUN = INSNUM
                  LLARR = 0
                  END IF
               END IF
C                                       read uv data loop
 100        CONTINUE
               IF (.NOT.GOTVIS) THEN
                  IRET = 0
                  CALL DATGET (RPARM, DBUFF, TIMLST, IRET)
                  ISEOF = (IRET.EQ.4)
                  IF (ISEOF) GO TO 200
                  IF (IRET.NE.0) THEN
                     IF (IRET.EQ.4) THEN
                        MSGTXT = 'NO DATA SELECTED, CHECK ADVERBS'
                     ELSE
                        WRITE (MSGTXT,1010) 'READ INPUT UV', IRET
                        END IF
                     IRET = 5
                     GO TO 990
                  ELSE
                     II = IVIS
                     DO 104 KIF = BIF,EIF
                        DO 103 J = 1,LCHINC
                           KCH = BCHAN + (J-1)*CHINC
                           DO 102 KP = 1,NSTOK
                              II = II + 3
                              VBUFF(II+1) = 0.0
                              VBUFF(II+2) = 0.0
                              VBUFF(II+3) = 0.0
                              DO 101 K = KCH,KCH+NCHAVG-1
                                 IJ = LNCS * ISOFF(KP) +
     *                              LNCF * (K-BCHAN) + LNCIF * (KIF-BIF)
                                 WT = DBUFF(IJ+3)
                                 IF (WT.GT.0.0) THEN
                                    VBUFF(II+1) = VBUFF(II+1) + WT *
     *                                 DBUFF(IJ+1)
                                    VBUFF(II+2) = VBUFF(II+2) + WT *
     *                                 DBUFF(IJ+2)
                                    VBUFF(II+3) = VBUFF(II+3) + WT
                                    END IF
 101                             CONTINUE
                              WT = VBUFF(II+3)
                              IF (WT.GT.0.0) THEN
                                 VBUFF(II+1) = VBUFF(II+1) / WT
                                 VBUFF(II+2) = VBUFF(II+2) / WT
                                 END IF
 102                          CONTINUE
 103                       CONTINUE
 104                    CONTINUE
                     END IF
                  END IF
C                                       get source number
               IERR = 0
               IRET = 0
               LSUNUM = 1
               IF (MULTIS) THEN
                  IF (ILOCSU.GE.0) THEN
                     LSUNUM = RPARM(ILOCSU+1) + 0.1
                  ELSE
                     IF (NSOUWD.EQ.1) LSUNUM = SOUWAN(1)
                     END IF
                  END IF
               GOTVIS = T
C                                       End of accumulation, save last
C                                       record
               TEMP = RPARM(1+ILOCT)
               IF ((TEMP.GE.TIMEND) .OR. (NOVIS.GE.(MXVIS-1))) THEN
                  DO 105 IA = 1,LIMIT
                     VISSAV(IA) = VBUFF(IVIS+3+IA)
 105                 CONTINUE
                  GO TO 200
                  END IF
               GOTVIS = F
               IF (ILOCB.GE.0) THEN
                  TEMP = RPARM(ILOCB+1)
                  JANT = TEMP + 0.1
                  IARR = 100.0 * (TEMP - JANT) + 1.49
                  IANT = JANT / 256
                  JANT = JANT - 256 * IANT
               ELSE
                  IANT = RPARM(1+ILOCA1) + 0.1
                  JANT = RPARM(1+ILOCA2) + 0.1
                  IARR = RPARM(1+ILOCSA) + 0.1
                  END IF
               IF (IANT.GT.JANT) THEN
                  J = IANT
                  IANT = JANT
                  JANT = J
                  END IF
C                                       Save record, 1=IANT, 2=JANT,
C                                       3=array, 4=time
               VBUFF(IVIS) = IANT
               VBUFF(IVIS+1) = JANT
               VBUFF(IVIS+2) = IARR
               TEMP = RPARM(1+ILOCT)
               VBUFF(IVIS+3) = TEMP
C                                       Do we want it?
               IF (.NOT.REQBAS (IANT, JANT, DESEL, IXANT, NXANT, IXBASL,
     *            NXBASL)) THEN
                  NANTSK = NANTSK + MUMBUF
                  GO TO 100
                  END IF
C                                       Check source
               IF (MULTIS) THEN
                  IF (LLSUN.LE.0) THEN
                     LLSUN = LSUNUM
                     LLARR = IARR
                  ELSE
                     IF (IARR.LT.LLARR) THEN
                        LLSUN = LSUNUM
                        LLARR = IARR
                        END IF
C                                       Different source in integration
                     IF ((IARR.EQ.LLARR) .AND. (LLSUN.NE.LSUNUM)) THEN
                        WASSOU = .TRUE.
                        GO TO 100
                        END IF
                     END IF
                  END IF
C                                       Vis wanted
               NOVIS = NOVIS + 1
               IVIS = IVIS + LBVIS
               GO TO 100
C                                       End of vis buffer reading loop.
 200           IF ((NOVIS.LE.0) .AND. (.NOT.ISEOF)) GO TO 305
               IF ((NOVIS.LE.0) .AND. (ISEOF)) GO TO 450
C                                       Get column numbers
               IVIS = 1
C                                       Data on grid twice
               IF (DOTWO) THEN
                  DO 210 IV = 1,NOVIS
                     ICOL1(IV) = -1
                     ICOL2(IV) = -1
                     IANT = VBUFF(IVIS) + 0.5
                     JANT = VBUFF(IVIS+1) + 0.5
                     IARR = VBUFF(IVIS+2) + 0.5
                     ITEMP = NUMAN(513+IARR) + JANT +
     *                  (IANT-1) * (NUMAN(1+IARR) + 1) - COLCRV
                     IF ((ITEMP.GE.1) .AND.
     *                  (ITEMP.LE.CATIMG(KINAX))) ICOL1(IV) = ITEMP
                     ITEMP = NUMAN(513+IARR) +
     *                  (JANT-1) * (NUMAN(1+IARR) + 1) + IANT - COLCRV
                     IF ((ITEMP.GE.1) .AND.
     *                  (ITEMP.LE.CATIMG(KINAX))) ICOL2(IV) = ITEMP
                     IVIS = IVIS + LBVIS
 210                 CONTINUE
C                                       Data on grid once
               ELSE
                  DO 220 IV = 1,NOVIS
                     ICOL1(IV) = -1
                     ICOL2(IV) = -1
                     IANT = VBUFF(IVIS) + 0.5
                     JANT = VBUFF(IVIS+1) + 0.5
                     IARR = VBUFF(IVIS+2) + 0.5
                     ITEMP = NUMAN(513+IARR) + JANT - IANT  - COLCRV +
     *                  (IANT-1) * (2*NUMAN(1+IARR) + 4 - IANT) / 2 + 1
                     IF ((ITEMP.GE.1) .AND. (ITEMP.LE.CATIMG(KINAX)))
     *                  ICOL1(IV) = ITEMP
                     IVIS = IVIS + LBVIS
 220                 CONTINUE
                  END IF
C                                       row (all data should be on the
C                                       same row)
               IVIS = IVIS - LBVIS
               TEMP = VBUFF(IVIS+3)
               IF ((IROW.LT.1) .OR. (IROW.GT.MROW)) IROW = 1
               DO 225 LR = IROW,MROW
                  IF ((TEMP.GE.MTIMES(LR)) .AND.
     *               (TEMP.LT.MTIMES(LR+1))) GO TO 226
 225              CONTINUE
               NFAIL = NFAIL + NOVIS * MUMBUF
               GO TO 305
 226           IROW = LR
               TIMEND = MTIMES(IROW+1)
C                                       Off grid
               IF ((IROW.LT.1) .OR. (IROW.GE.CATIMG(KINAX+1))) THEN
                  NFAIL = NFAIL + NOVIS * MUMBUF
                  GO TO 305
C                                       Check if some fell off grid in
C                                       baseline.
               ELSE
                  DO 230 IV = 1,NOVIS
                     IF ((ICOL1(IV).LE.0) .AND. (ICOL2(IV).LE.0)) THEN
                        NFAIL = NFAIL + MUMBUF
                     ELSE
                        NPOINT = NPOINT + MUMBUF
                        END IF
 230                 CONTINUE
                  END IF
C                                       Average, write row
               IF (IROW.GT.NROW) THEN
                  CALL GTBWRT (OLDSOU, MSOU, MTIMES, NCOL2, NCOLMX,
     *               IOBUF, ITYP, LUN, FIND, F, IROW, NROW, MUMBUF,
     *               RBUFF, SBUFF, NBUFF, IBPTR, RMAX, RMIN, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1230) IRET
                     IRET = 4
                     GO TO 990
                     END IF
                  TIMEND = -1.0E20
                  END IF
C                                       Get source flux
               IF ((.NOT.ISINGL) .AND. (LSTSOU.NE.LLSUN)) THEN
                  CALL GETSOU (LLSUN, IUDISK, IUCNO, CATSAV, LUNO, IERR)
                  DAFLUX(1) = 1.0
                  FLUXX = FLUX(1,LIF)
                  IF (FLUXX.LE.1.0E-20) FLUXX = 1.0
                  IF (IERR.EQ.0) DAFLUX(1) = 1.0 / FLUXX
                  DO 240 IBUFF = 2,MUMBUF
                     DAFLUX(IBUFF) = 1.0
C                                       Multiple channels
                     IF ((IERR.EQ.0) .AND. (LCHINC.GT.1)) THEN
                        DAFLUX(IBUFF) = 1.0 / FLUXX
                        END IF
C                                       Multiple IFs
                     IF ((IERR.EQ.0) .AND. (LIFINC.GT.1)) THEN
                        FLUXX = FLUX(1,LIF+(IBUFF-1)/LSTINC)
                        IF (FLUXX.LE.1.0E-20) FLUXX = 1.0
                        DAFLUX(IBUFF) = 1.0 / FLUXX
                        END IF
 240                 CONTINUE
                  LSTSOU = LLSUN
                  END IF
C                                       Process visibility data.
C                                       First entry:
C                                       Loop over buffer
               DO 300 IBUFF = 1,MUMBUF
                  IVIS = 5 + (IBUFF-1) * 3
C                                       Divide by source flux
                  IF (ABS (DAFLUX(IBUFF)-1.0).GT.1.0E-5) THEN
                     VALUE1 = DAFLUX(IBUFF)
                     DO 245 IV = 1,NOVIS
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        IVIS = IVIS + LBVIS
 245                    CONTINUE
                     END IF
                  IVIS = 5 + (IBUFF-1) * 3
                  IVISO = IVIS - 4
C                                       Amp (scalar avg) or rms
                  IF ((ITYP.EQ.0).OR.(ITYP.EQ.3).OR.(ITYP.EQ.4)) THEN
      DO 250 IV = 1,NOVIS
         IF ((ICOL1(IV).GE.1) .AND. (VBUFF(IVIS+2).GT.0.0)) THEN
            K = ICOL1(IV)
            J = IBPTR(IBUFF) + (ICOL1(IV)-1) * COLMUL + COLUP
            IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF) = 0.0
            VALUE1 = VBUFF(IVIS)
            VALUE2 = VBUFF(IVIS+1)
            VALUE3 =  SQRT (VALUE1*VALUE1 + VALUE2*VALUE2)
            RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VALUE3
            SBUFF(K,IBUFF) = SBUFF(K,IBUFF) + VALUE3 * VALUE3
            NBUFF(K,IBUFF) = NBUFF(K,IBUFF) + 1
            END IF
         IVIS = IVIS + LBVIS
 250     CONTINUE
C                                       other data types
                  ELSE
      DO 260 IV = 1,NOVIS
         IF ((ICOL1(IV).GE.1) .AND. (VBUFF(IVIS+2).GT.0.0)) THEN
C                                       time
            J = IBPTR(IBUFF)
            IF (ITYP.EQ.7) THEN
               IF (RBUFF(J,IBUFF).EQ.FBLANK) THEN
                  RBUFF(J,IBUFF) = 0.0
                  RBUFF(J+1,IBUFF) = 0.0
                  END IF
               RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + 1.0
               RBUFF(J+1,IBUFF) = RBUFF(J+1,IBUFF) + VBUFF(IVIS-IVISO)
               END IF
C                                       data
            K = ICOL1(IV)
            J = J + (ICOL1(IV)-1) * COLMUL + COLUP
            IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF) = 0.0
            RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VBUFF(IVIS)
            SBUFF(K,IBUFF) = SBUFF(K,IBUFF) + VBUFF(IVIS+1)
            NBUFF(K,IBUFF) = NBUFF(K,IBUFF) + 1
            END IF
         IVIS = IVIS + LBVIS
 260     CONTINUE
                     END IF
C                                       Second entry:
C                                       Loop over buffer
                  IF (DOTWO) THEN
                     IVIS = 5 + (IBUFF-1) * 3
                     IVISO = IVIS - 4
                     IF ((ITYP.EQ.0).OR.(ITYP.EQ.3).OR.(ITYP.EQ.4)) THEN
C                                       Amp (scalar avg) or rms
      DO 270 IV = 1,NOVIS
         IF ((ICOL2(IV).GE.1) .AND. (VBUFF(IVIS+2).GT.0.0)) THEN
            K = ICOL2(IV)
            J = IBPTR(IBUFF) + (ICOL2(IV)-1) * COLMUL + COLUP
            IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF) = 0.0
            VALUE1 = VBUFF(IVIS)
            VALUE2 = VBUFF(IVIS+1)
            VALUE3 =  SQRT (VALUE1*VALUE1 + VALUE2*VALUE2)
            RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VALUE3
            SBUFF(K,IBUFF) = SBUFF(K,IBUFF) + VALUE3 * VALUE3
            NBUFF(K,IBUFF) = NBUFF(K,IBUFF) + 1
            END IF
         IVIS = IVIS + LBVIS
 270     CONTINUE
C                                       other data types
                  ELSE
      DO 280 IV = 1,NOVIS
         IF ((ICOL2(IV).GE.1) .AND. (VBUFF(IVIS+2).GT.0.0)) THEN
C                                       time
            J = IBPTR(IBUFF)
            IF (ITYP.EQ.7) THEN
               IF (RBUFF(J,IBUFF).EQ.FBLANK) THEN
                  RBUFF(J,IBUFF) = 0.0
                  RBUFF(J+1,IBUFF) = 0.0
                  END IF
               RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + 1.0
               RBUFF(J+1,IBUFF) = RBUFF(J+1,IBUFF) + VBUFF(IVIS-IVISO)
               END IF
C                                       data
            K = ICOL2(IV)
            J = J + (ICOL2(IV)-1) * COLMUL + COLUP
            IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF) = 0.0
            RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VBUFF(IVIS)
            SBUFF(K,IBUFF) = SBUFF(K,IBUFF) + VBUFF(IVIS+1)
            NBUFF(K,IBUFF) = NBUFF(K,IBUFF) + 1
            END IF
         IVIS = IVIS + LBVIS
 280     CONTINUE
                        END IF
                     END IF
 300              CONTINUE
C                                       Restore first record
 305              DO 310 IA = 1,LIMIT
                     VBUFF(4+IA) = VISSAV(IA)
 310                 CONTINUE
C                                       Get next vis buffer load
                  TIMEND = -1.0E20
                  NOVIS = 0
                  IVIS = 1
                  IF (LLSUN.GT.0) OLDSOU = LLSUN
                  IF (.NOT.ISEOF) GO TO 90
C                                       write rest of plane(s)
 450        IROW = CATIMG(KINAX+1)
            CALL GTBWRT (OLDSOU, MSOU, MTIMES, NCOL2, NCOLMX, IOBUF,
     *         ITYP, LUN, FIND, T, IROW, NROW, MUMBUF, RBUFF, SBUFF,
     *         NBUFF, IBPTR, RMAX, RMIN, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1230) IRET
               IRET = 4
               GO TO 990
               END IF
C                                       Close the uv I/O too
            CALL UVGET ('CLOS', RPARM, VBUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'CLOSE INPUT UV', IRET
               GO TO 990
               END IF
C                                       End of channed loop
 480        CONTINUE
C                                       End of IF loop
 500     CONTINUE
C                                       Restore IF's to input values
      BIF = LBIF
      EIF = LEIF
      BCHAN = LBCHAN
      ECHAN = LECHAN
C                                       Close file(s)
      DO 520 I = 1,NUMBUF
         CALL ZCLOSE (LUN(I), FIND(I), JERR)
 520     CONTINUE
C                                       finish the header
      CALL COPY (256, CATIMG, CATBLK)
      CATR(KRDMX) = RMAX
      CATR(KRDMN) = RMIN
      CATD(KDCRV+1) = CATD(KDCRV+1) * 24.D0 * 3600.D0
      CATR(KRCIC+1) = CATR(KRCIC+1) * 24. * 3600.
      CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', VBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1520) IERR
         CALL MSGWRT (6)
         END IF
C                                       Summary messages
      WRITE (MSGTXT,1521) NPOINT
      CALL MSGWRT (4)
      WRITE (MSGTXT,1522) NFAIL
      CALL MSGWRT (4)
      WRITE (MSGTXT,1523) NANTSK
      CALL MSGWRT (4)
      IF (WASSOU) THEN
         MSGTXT = 'Some data were dropped to avoid averaging different'
     *      // ' sources!'
         CALL MSGWRT (4)
         END IF
      IF ((NPOINT.GT.0.0D0) .AND. (LNCFIL.GT.0)) FRW(LNCFIL) = 1
      IF (NPOINT.LE.0.0D0) IRET = 6
      GO TO 999
C                                       Error message print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRIDTB: CALLED WITH NAXBUF =',I3,' > THE 8 SUPPORTED')
 1005 FORMAT ('GRIDTB: SORT ORDER ''',A2,''' NOT FULLY RECOGNIZED',
     *   ' - USE UVSRT')
 1010 FORMAT ('GRIDTB: UNABLE TO ',A,' FILE - ERROR',I5)
 1015 FORMAT ('GRIDTB: UNABLE TO READ ANTENNAS INFO - ERROR',2I5)
 1055 FORMAT ('GRIDTB: GRID REQUIRES',I5,' COLUMNS, ONLY',I5,
     *   ' AVAILABLE.')
 1070 FORMAT ('GRIDTB WARNING: using ECHAN',I5,' not input ECHAN',I5)
 1230 FORMAT ('GRIDTB: ERROR',I5,' FROM GTBWRT')
 1520 FORMAT ('ERROR',I5,' UPDATING THE CATALOG HEADER')
 1521 FORMAT ('Included',F13.0,' points in the grid')
 1522 FORMAT ('Dropped ',I12,'  points off the grid')
 1523 FORMAT ('Dropped ',I12,'  points due to antennas/baseline')
      END
