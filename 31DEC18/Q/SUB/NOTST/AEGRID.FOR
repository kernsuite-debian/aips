      SUBROUTINE AEGRID (OLUN, ILUN, MIDSK, MIFIL, MODSK, MOFIL, RWT,
     *   BLC, TRC, CATIN, JBUFSZ, BUFF1, BUFF2, MBUFSZ, BUFFM, OBUFSZ,
     *   OUTBUF, MAXCWT, XNLIM, IRET)
C-----------------------------------------------------------------------
C! Regrids image having coordinates in an image
C# Singledish AP
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2000, 2006
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
C   AEGRID regrids an image using images of the relative RA and Dec of
C   the input pixels.  It assumes that the DSDG common has been prepared
C   for the gridding.
C   Inputs:
C      OLUN     I      LUN to use for output
C      ILUN     I      LUN to use for input
C      MIDSK    I      > 0 => Disk # of cataloged input file
C                      = 0 => SC file read with MINIT/MDISK
C      MIFIL    I      CNO for cataloged or SC index
C      MODSK    I      > 0 => disk # for cataloged MA, else SC
C      MOFIL    I      CNO for cataloged, index to SC
C      RWT      R      Fractional cutoff level
C      BLC      I(7)   BLC of input image
C      TRC      I(7)   TRC of input image - see note below
C      CATIN    I(256) Header of input image
C      JBUFSZ   I      Size of BUFF1, BUFF2 in AIPS bytes (2*UVBFSS)
C      MBUFSZ   I      Size of BUFFM in AIPS bytes (2*4*MAXCIF)
C      OBUFSZ   I      Size of OUTBUF in AIPS bytes (2*8192)
C   Outputs:
C      BUFF1    R(*)   UV buffer
C      BUFF2    R(*)   UV buffer scratch
C      BUFFM    R(*)   Data load buffer
C      OUTBUF   R(*)   Image output buffer
C      MAXCWT   R      Maximum convolved weight
C      XNLIM    R      Cutoff on sum of convolved weights
C      IRET     I      Error code: 0 -> ok
C   Common inputs:
C      NX       I      X dimension out
C      NY       I      Y dimension
C      SDBCHN   I      Begin channel
C      SDECHN   I      End channel
C      CATMAP   I(256) Image header
C      CXTYPE   I      X convolution type
C      XPARM    R(10)  X convolution parameters
C      CYTYPE   I      Y convolution type
C      YPARM    R(10)  Y convolution parameters
C   coordinate data are assumed to be in planes TRC(3)+1 and +2.
C-----------------------------------------------------------------------
      INTEGER   ILUN, OLUN, MIDSK, MIFIL, MODSK, MOFIL, BLC(7), TRC(7),
     *   CATIN(256), JBUFSZ, MBUFSZ, OBUFSZ, IRET
      REAL      RWT, MAXCWT, XNLIM, BUFF1(*), BUFF2(*), BUFFM(*),
     *   OUTBUF(*)
C
      INTEGER   NCHAN, IC, IERR, IG, BO, BLKOF, DEPTH(5), WIN(4), IFACT,
     *   DIND, NO2, M, LAPREC, INCNT, GRID(3), IY, IDATA, II, UVPTR,
     *   VISPTR, WTPTR, BIND, TOLROW, INC, APSIZ, KAP, CX, CY, CINC,
     *   MAXREC, KNPTR, JJ, KK, ITEMP, INDL, NUMKEP, NUMOFF, PRJERR,
     *   IDEP(5), I1, I2, I3, I4, I5, I6, I7, INX, INY, OWIN(4), NEED
      REAL      DMAX, DMIN, CATMAR(256), XPIX, YPIX
      DOUBLE PRECISION XX, YY, RA0, DEC0
      LOGICAL   T, F, DOABS, WASBLK
      CHARACTER PHNAME*48
      INCLUDE 'INCS:DSDG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (CATMAP, CATMAR)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, UVPTR /1, 16/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      WASBLK = .FALSE.
      MAXCWT = 0.0
      PRJERR = 0
C                                       Make sure an ODD number of rows
C                                       is being kept in the AP.
      NO2 = MAX (XPARM(1), 1.0) + 0.1
      M = MAX (YPARM(1), 1.0) + 0.1
      M = M * 2 + 1
      IFACT = 2
      IF (CXTYPE.LE.10) THEN
         INC = 100 * (M + 1 + 2*NO2) + 2
      ELSE
         INC = XPARM(5) + 0.1
         INC = INC * M + 1
         INC = INC * INC
         END IF
      INX = TRC(1) - BLC(1) + 1
      INY = TRC(2) - BLC(2) + 1
      IG = INX * INY
      NEED = INC + 16 + MBUFSZ/4 + 2 * IG + IFACT*NX*NY + 100
      NEED = NEED / 1024 + 4
      CALL QINIT (NEED, 0, KAP)
      APSIZ = PSAPNW * 1024
      INC = APSIZ - INC - 16 - MBUFSZ/4 - 2 * IG - IFACT*NX*NY
      IF (INC.LE.100) THEN
         MSGTXT = 'AEGRID: AP MEMORY TOO SMALL FOR SUCH LARGE PLANES'
         IRET = 1
         GO TO 990
         END IF
C                                       Open files - output image
      NCHAN = CATMAP(KINAX+2)
      IF (MODSK.LE.0) THEN
         CALL ZPHFIL ('SC', SCRVOL(MOFIL), SCRCNO(MOFIL), 1, PHNAME,
     *      IERR)
         CALL ZOPEN (OLUN, INDL, SCRVOL(MOFIL), PHNAME, T, F, T, IRET)
      ELSE
         CALL ZPHFIL ('MA', MODSK, MOFIL, 1, PHNAME, IERR)
         CALL ZOPEN (OLUN, INDL, MODSK, PHNAME, T, F, T, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Open input image
      IF (MIDSK.LE.0) THEN
         CALL ZPHFIL ('SC', SCRVOL(MIFIL), SCRCNO(MIFIL), 1, PHNAME,
     *      IERR)
         CALL ZOPEN (ILUN, DIND, SCRVOL(MIFIL), PHNAME, T, T, T, IRET)
      ELSE
         CALL ZPHFIL ('MA', MIDSK, MIFIL, 1, PHNAME, IERR)
         CALL ZOPEN (ILUN, DIND, MIDSK, PHNAME, T, F, T, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Setup for AP griding
C                                       Set AP pointers
      IG = INX * INY
      GRID(3) = APSIZ - IG - 1
      GRID(2) = GRID(3) - IG
      IG = IFACT * NX * NY
      GRID(1) = GRID(2) - IG
C                                       Clear AP
      IF (CXTYPE.LE.10) THEN
         CINC = 100
         CX = GRID(1) - CINC * (2 * NO2 + 1) - 1
         CY = CX - CINC * M - 1
      ELSE
         CINC = XPARM(5) + 0.1
         CX = GRID(1) - (CINC * M + 1) * (CINC * M + 1) - 1
         CY = CX
         END IF
      DOABS = RWT.LE.0.0
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      LAPREC = 3 + 2
      MAXREC = (CY - 16) / LAPREC - 5
C                                       Be sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         IG = - MAXR EC * LAPREC
         WRITE (MSGTXT,1020) IG
         IRET = 1
         GO TO 990
         END IF
C                                       Set gridding convolution tables
      CALL CONVFN (CX, CXTYPE, XPARM, BUFF2)
      IF (CXTYPE.LE.10) CALL CONVFN (CY, CYTYPE, YPARM, BUFF2)
      TOLROW = 2 * NX
      WIN(1) = BLC(1)
      WIN(2) = BLC(2)
      WIN(3) = TRC(1)
      WIN(4) = TRC(2)
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = NX
      OWIN(4) = NY
      DMAX = -1.0E20
      DMIN = 1.0E20
      NUMKEP = 0
      NUMOFF = 0
C                                       Loop over input cube
      DO 700 I7 = BLC(7),TRC(7)
      DO 700 I6 = BLC(6),TRC(6)
      DO 700 I5 = BLC(5),TRC(5)
      DO 700 I4 = BLC(4),TRC(4)
C                                       Read RA, dec images
         IDEP(2) = I4
         IDEP(3) = I5
         IDEP(4) = I6
         IDEP(5) = I7
         DO 30 IC = 1,2
            IDEP(1) = TRC(3) + IC
            CALL COMOFF (CATIN(KIDIM), CATIN(KINAX), IDEP, BLKOF, IERR)
            BLKOF = BLKOF + BO
            CALL MINIT ('READ', ILUN, DIND, CATIN(KINAX),
     *         CATIN(KINAX+1), WIN, BUFF1, JBUFSZ, BLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT'
               GO TO 990
               END IF
            IDATA = GRID(1+IC)
            DO 20 IY = 1,INY
               CALL MDISK ('READ', ILUN, DIND, BUFF1, BIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE'
                  GO TO 990
                  END IF
C                                       Read out gridded data.
               CALL QPUT (BUFF1(BIND), IDATA, INX, 2)
               IDATA = IDATA + INX
               CALL QWAIT
 20            CONTINUE
 30         CONTINUE
C                                       Set coordinate computation
         DEPTH(1) = 1
         DEPTH(2) = I4 - BLC(4) + 1
         DEPTH(3) = I5 - BLC(5) + 1
         DEPTH(4) = I6 - BLC(6) + 1
         DEPTH(5) = I7 - BLC(7) + 1
         CALL COPY (256, CATBLK, BUFF1)
         CALL COPY (256, CATMAP, CATBLK)
         IF (LOCNUM.LE.0) LOCNUM = 1
         CALL SETLOC (DEPTH, .FALSE.)
         CALL COPY (256, CATIN, CATBLK)
         RA0 = CATD(KDORA)
         DEC0 = CATD(KDODE)
         CALL COPY (256, BUFF1, CATBLK)
         IDATA = GRID(2)
         IY = GRID(3)
C                                       The divide by COS(DEC) was done
C                                       by OTFBS, not needed here.
         DO 50 I2 = 1,INY
            CALL QGET (BUFF1, IDATA, INX, 2)
            CALL QGET (BUFF2, IY, INX, 2)
            CALL QWAIT
            DO 40 I1 = 1,INX
               IF ((BUFF1(I1).NE.FBLANK) .AND. (BUFF2(I1).NE.FBLANK))
     *            THEN
                  XX = RA0 + BUFF1(I1)
                  YY = DEC0 + BUFF2(I1)
                  CALL XYPIX (XX, YY, BUFF1(I1), BUFF2(I1), IRET)
                  IF (IRET.NE.0) THEN
                     PRJERR = PRJERR + 1
                     IF (PRJERR.LE.1) THEN
                        WRITE (MSGTXT,1100) IRET
                        CALL MSGWRT (7)
                        END IF
                     NUMOFF = NUMOFF + 1
                     BUFF1(I1) = FBLANK
                     BUFF2(I1) = FBLANK
                     END IF
                  END IF
 40            CONTINUE
            CALL QPUT (BUFF1, IDATA, INX, 2)
            CALL QPUT (BUFF2, IY, INX, 2)
            CALL QWAIT
            IDATA = IDATA + INX
            IY = IY + INX
 50         CONTINUE
C                                       Loop over channels
         DO 300 I3 = BLC(3),TRC(3)
            IDEP(1) = I3
            DEPTH(1) = I3 - BLC(3) + 1
            CALL QVCLR (GRID(1), 1, IG)
            LAPREC = 2 + 3
            MAXREC = (CY - 16) / LAPREC - 5
            IC = MBUFSZ / (2 * LAPREC)
            MAXREC = MIN (MAXREC, IC)
C                                       Init input read
            CALL COMOFF (CATIN(KIDIM), CATIN(KINAX), IDEP, BLKOF, IERR)
            BLKOF = BLKOF + BO
            CALL MINIT ('READ', ILUN, DIND, CATIN(KINAX),
     *         CATIN(KINAX+1), WIN, BUFF1, JBUFSZ, BLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT'
               GO TO 990
               END IF
            INCNT = 0
            KNPTR = 1
            DO 70 I2 = 1,INY
C                                       Get coordinates
               IDATA = GRID(2) + (I2 - 1) * INX
               CALL QGET (BUFF2(1), IDATA, INX, 2)
               IDATA = GRID(3) + (I2 - 1) * INX
               CALL QGET (BUFF2(INX+1), IDATA, INX, 2)
               CALL MDISK ('READ', ILUN, DIND, BUFF1, BIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ'
                  GO TO 990
                  END IF
               CALL QWAIT
C                                       Move to buffer
               DO 60 I1 = 1,INX
                  XPIX = BUFF2(I1)
                  YPIX = BUFF2(I1+INX)
                  IF ((BUFF1(BIND+I1-1).NE.FBLANK) .AND.
     *               (XPIX.NE.FBLANK) .AND. (YPIX.NE.FBLANK)) THEN
C                                       Does not fit
                     IF ((XPIX.LT.0.5+XPARM(1)) .OR.
     *                  (XPIX.GT.NX+0.5-XPARM(1)) .OR.
     *                  (YPIX.LT.0.5+YPARM(1)) .OR.
     *                  (YPIX.GT.NY+0.5-YPARM(1))) THEN
                        NUMOFF = NUMOFF + 1
                     ELSE
                        NUMKEP = NUMKEP + 1
C                                       Y in u position
                        BUFFM(KNPTR) = YPIX
C                                       X offset in v position
                        BUFFM(KNPTR+1) = XPIX - NX/2 - 1.
                        BUFFM(KNPTR+2) = 1.0
                        KNPTR = KNPTR + 3
                        BUFFM(KNPTR) = BUFF1(BIND+I1-1)
                        BUFFM(KNPTR+1) = 1.0
                        KNPTR = KNPTR + 2
                        INCNT = INCNT + 1
C                                       Load into AP.
                        IF (INCNT.GE.MAXREC) THEN
                           ITEMP = KNPTR - 1
                           CALL QWR
                           CALL QPUT (BUFFM, UVPTR, ITEMP, 2)
                           CALL QWAIT
C                                       Do the gridding
                           VISPTR = UVPTR + 2 + 1
                           WTPTR = VISPTR + 1
                           CALL QGRD6 (UVPTR, VISPTR, WTPTR, GRID(1),
     *                        CY, CX, CINC, NO2, M, NX, LAPREC, INCNT)
                           CALL QWR
                           INCNT = 0
                           KNPTR = 1
                           END IF
                        END IF
                     END IF
 60               CONTINUE
 70            CONTINUE
C                                       Finish last buffer
C                                       Load into AP.
            IF (INCNT.GT.0) THEN
               ITEMP = INCNT * LAPREC
               CALL QWR
               CALL QPUT (BUFFM, UVPTR, ITEMP, 2)
               CALL QWAIT
C                                       Do the gridding
               VISPTR = UVPTR + 2 + 1
               WTPTR = VISPTR + 1
               CALL QGRD6 (UVPTR, VISPTR, WTPTR, GRID(1), CY, CX, CINC,
     *            NO2, M, NX, LAPREC, INCNT)
               CALL QWR
               END IF
C                                       Summary
            WRITE (MSGTXT,1125) NUMKEP, NUMOFF
            IF (I3.EQ.BLC(3)) CALL MSGWRT (4)
            WRITE (MSGTXT,1126) PRJERR
            IF (PRJERR.GT.0) CALL MSGWRT (7)
            IF (NUMKEP.LE.0.0) IRET = 9
            IF (IRET.NE.0) GO TO 999
C                                       Done gridding find max cwt
            CALL QWAIT
C                                       Loop over plane
            IDATA = GRID(1)
            DO 80 IY = 1,NY
C                                       Read out gridded data.
               CALL QGET (BUFF2, IDATA, TOLROW, 2)
               IDATA = IDATA + TOLROW
               CALL QWAIT
C                                       Max weight
               KK = 0
               DO 75 II = 1,NX
                  KK = KK + 2
                  MAXCWT = MAX (MAXCWT ,ABS(BUFF2(KK)))
 75               CONTINUE
 80            CONTINUE
            IF (MAXCWT.LE.0.0) MAXCWT = 1.0
            XNLIM = ABS (RWT) * MAXCWT
C                                       Done gridding, write planes
            CALL QWAIT
            CALL COMOFF (CATMAP(KIDIM), CATMAP(KINAX), DEPTH, BLKOF,
     *         IERR)
            BLKOF = BLKOF + BO
            CALL MINIT ('WRIT', OLUN, INDL, NX, NY, OWIN, OUTBUF,
     *         OBUFSZ, BLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'INIT'
               GO TO 990
               END IF
C                                       Loop over plane
            IDATA = GRID(1)
            DO 190 IY = 1,NY
C                                       Write
               CALL MDISK ('WRIT', OLUN, INDL, OUTBUF, BIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'WRITE'
                  GO TO 990
                  END IF
C                                       Read out gridded data.
               CALL QGET (BUFF2, IDATA, TOLROW, 2)
               IDATA = IDATA + TOLROW
               CALL QWAIT
C                                       Move data: validity image
               JJ = BIND - 1
               KK = -1
C                                       Move data: Interpolation
               IF (DOABS) THEN
                  DO 160 II = 1,NX
                     JJ = JJ + 1
                     KK = KK + 2
                     IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                        OUTBUF(JJ) = BUFF2(KK) / BUFF2(KK+1)
                        DMAX = MAX (DMAX, OUTBUF(JJ))
                        DMIN = MIN (DMIN, OUTBUF(JJ))
                     ELSE
                        OUTBUF(JJ) = FBLANK
                        WASBLK = .TRUE.
                        END IF
 160                 CONTINUE
               ELSE
                  DO 165 II = 1,NX
                     JJ = JJ + 1
                     KK = KK + 2
                     IF (BUFF2(KK+1).GE.XNLIM) THEN
                        OUTBUF(JJ) = BUFF2(KK) / BUFF2(KK+1)
                        DMAX = MAX (DMAX, OUTBUF(JJ))
                        DMIN = MIN (DMIN, OUTBUF(JJ))
                     ELSE
                        OUTBUF(JJ) = FBLANK
                        WASBLK = .TRUE.
                        END IF
 165                 CONTINUE
                  END IF
 190           CONTINUE
            CALL MDISK ('FINI', OLUN, INDL, OUTBUF, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'FINISH'
               GO TO 990
               END IF
 300        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL QRLSE
      CATMAR(KRDMX) = DMAX
      CATMAR(KRDMN) = DMIN
      IF (WASBLK) THEN
         CATMAR(KRBLK) = FBLANK
      ELSE
         CATMAR(KRBLK) = 0.0
         END IF
      CALL ZCLOSE (ILUN, DIND, IERR)
      CALL ZCLOSE (OLUN, INDL, IERR)
      WRITE (MSGTXT,1315) MAXCWT
      CALL MSGWRT (5)
      WRITE (MSGTXT,1316) XNLIM
      CALL MSGWRT (5)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AEGRID: ERROR',I4,1X,A,'ING INPUT')
 1010 FORMAT ('AEGRID: ERROR',I4,1X,A,'ING OUTPUT')
 1020 FORMAT ('AEGRID:',I8,' TOO FEW AP WORDS AVAILABLE')
 1100 FORMAT ('AEGRID: ERROR',I5,' PROJECTING POSITION')
 1125 FORMAT ('AEGRID:',I9,' points kept;',I9,
     *   ' others fell off the grid')
 1126 FORMAT ('AEGRID:',I9,' ERRORS PROJECTING COORDINATES')
 1315 FORMAT ('AEGRID: Maximum convolved weight =',1PE12.4)
 1316 FORMAT ('AEGRID;    used to scale cutoff to',1PE12.4)
      END
