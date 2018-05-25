      SUBROUTINE SDGRID (LUNL, UDSK, UFIL, MDSK, MFIL, DOPROJ, IWT, RWT,
     *   JBUFSZ, BUFF1, BUFF2, MBUFSZ, BUFFM, OBUFSZ, OUTBUF, MAXCWT,
     *   XNLIM, IRET)
C-----------------------------------------------------------------------
C! Grids single-dish "uv" data to a multi-channel image
C# Singledish AP
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1998, 2000, 2006
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
C   SDGRID grids SD data to an image
C   It assumes that the DSEL common has been set up so that, when UFIL >
C   0, SDGET can read the data.  File opens are handled internally as is
C   the call to SETLOC needed when DOPROJ true.
C   Inputs:
C      LUNL     I      LUN to use
C      UDSK     I      > 0 => Disk # of cataloged file read with
C                             SDGET
C                      = 0 => SC file read with UVINIT/UVDISK
C      UFIL     I      CNO for cataloged or SC index
C      MDSK     I      > 0 => disk # for cataloged MA, else SC
C      MFIL     I      CNO for cataloged, index to SC
C      DOPROJ   L      Convert RA/DEC to pixels
C      IWT      I      Type of image to make: -1 interpolated
C                         0 convolved, 1 weight, 2 1/sigma**2
C      RWT      R      Fractional cutoff level
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
C      NX       I      X dimension
C      NY       I      Y dimension
C      SDBCHN   I      Begin channel
C      SDECHN   I      End channel
C      VISNUM   I      Number of vis in data set
C      CATMAP   I(256) Image header
C      CXTYPE   I      X convolution type
C      XPARM    R(10)  X convolution parameters
C      CYTYPE   I      Y convolution type
C      YPARM    R(10)  Y convolution parameters
C   UV data is assumed to be x pixel, y pixel, overall weight, then
C   BCHAN-ECHAN worth of value, channel weight when DOPROJ is false.
C   When DOPROJ is true, then normal UV conventions apply.
C-----------------------------------------------------------------------
      INTEGER   LUNL, UDSK, UFIL, MDSK, MFIL, JBUFSZ, MBUFSZ, OBUFSZ,
     *   IWT, IRET
      REAL      RWT, MAXCWT, XNLIM, BUFF1(*), BUFF2(*), BUFFM(*),
     *   OUTBUF(*)
      LOGICAL   DOPROJ
C
      INTEGER   MXCH
      PARAMETER (MXCH = 256)
      INTEGER   NCHAN, NC, IC, IERR, NPASS, IPASS, LLREC, IG, ILENBU,
     *   BO, VO, BLKOF, DEPTH(5), WIN(4), IFACT, IR, DLUN, DIND, NUM,
     *   NO2, M, LAPREC, NIO, IBIND, INPTR, INCNT, GRID(MXCH), IY,
     *   IDATA, II, UVPTR, VISPTR, WTPTR, BIND, TOLROW, INC, APSIZ, KAP,
     *   CX, CY, CINC, MAXREC, ICHAN, KNPTR, JJ, KK, ITEMP, NVISIN,
     *   INDL, NUMKEP, NUMOFF, PRJERR, NEED
      REAL      DMAX, DMIN, CATMAR(256), XPIX, YPIX
      DOUBLE PRECISION XX, YY
      LOGICAL   T, F, DOABS, UISSCR, WASBLK
      CHARACTER PHNAME*48
      INCLUDE 'INCS:DSDG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (CATMAP, CATMAR)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO, UVPTR, DLUN /1, 0, 16, 4/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      WASBLK = .FALSE.
      MAXCWT = 0.0
      PRJERR = 0
      UISSCR = UDSK.LE.0
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
      NCHAN = CATMAP(KINAX+2)
      NC = MIN (MXCH, NCHAN)
      NEED = INC + 16 + MBUFSZ/4 + (IFACT*NX*NY + 20) * (NC + 1)
      NEED = NEED / 1024 + 2
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = INC + 16 + MBUFSZ/4 + (IFACT*NX*NY + 20) * 2
         NEED = NEED / 1024 + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            IRET = 1
            MSGTXT = 'SDGRID UNABLE TO GET NEEDED MEMORY'
            GO TO 990
            END IF
         END IF
      APSIZ = 1024 * PSAPNW
      INC = (APSIZ - INC - 16 - MBUFSZ/4) / (IFACT*NX*NY + 20)
      IF (INC.LE.0) THEN
         MSGTXT = 'SDGRID: AP MEMORY TOO SMALL FOR SUCH LARGE PLANES'
         CALL MSGWRT (8)
         MSGTXT = 'SDGRID: TRY USING SDIMG INSTEAD OF SDGRD'
         IRET = 1
         GO TO 990
         END IF
C                                       Open files - image planes
      NC = MIN (MXCH, NCHAN)
      NC = MIN (NC, INC)
      IF (MDSK.LE.0) THEN
         CALL ZPHFIL ('MA', SCRVOL(MFIL), SCRCNO(MFIL), 1, PHNAME, IERR)
         CALL ZOPEN (LUNL, INDL, SCRVOL(MFIL), PHNAME, T, F, T, IRET)
      ELSE
         CALL ZPHFIL ('MA', MDSK, MFIL, 1, PHNAME, IERR)
         CALL ZOPEN (LUNL, INDL, MDSK, PHNAME, T, F, T, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Open UV data
      IF (UISSCR) THEN
         CALL ZPHFIL ('SC', SCRVOL(UFIL), SCRCNO(UFIL), 1, PHNAME, IERR)
         CALL ZOPEN (DLUN, DIND, SCRVOL(UFIL), PHNAME, T, T, T, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Setup for AP griding
C                                       Set AP pointers
      IG = IFACT * NX * NY
      DO 20 IC = 1,NC
         GRID(IC) = APSIZ - (NC-IC+1) * IG - 1
 20      CONTINUE
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
      LAPREC = 3 + 2 * NC
      MAXREC = (CY - 16) / LAPREC - 5
C                                       Be sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         IG = - MAXREC * LAPREC
         WRITE (MSGTXT,1020) IG
         IRET = 1
         GO TO 990
         END IF
C                                       Set gridding convolution tables
      CALL CONVFN (CX, CXTYPE, XPARM, BUFF2)
      IF (CXTYPE.LE.10) CALL CONVFN (CY, CYTYPE, YPARM, BUFF2)
      TOLROW = 2 * NX
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      DMAX = -1.0E20
      DMIN = 1.0E20
C                                       loop over channels
      NPASS = (NCHAN - 1) / NC + 1
      CALL FILL (5, 1, DEPTH)
      IF (DOPROJ) THEN
         LLREC = NRPARM + 3 * NCHAN
         CALL COPY (256, CATBLK, BUFF1)
         CALL COPY (256, CATMAP, CATBLK)
         IF (LOCNUM.LE.0) LOCNUM = 1
         CALL SETLOC (DEPTH, .TRUE.)
         CALL COPY (256, BUFF1, CATBLK)
         NUMKEP = 0
         NUMOFF = 0
      ELSE
         LLREC = 3 + 2 * NCHAN
         END IF
      NVISIN = VISNUM
      DO 300 IPASS = 1,NPASS
         NUM = IG * NC
         CALL QVCLR (GRID(1), 1, NUM)
         ICHAN = (IPASS - 1) * NC + 1
         IC = ICHAN + NC - 1
         IF (IC.GT.NCHAN) THEN
            NC = NCHAN + 1 - ICHAN
            IC = NCHAN
            END IF
         II = ICHAN + SDBCHN - 1
         JJ = IC + SDBCHN - 1
         WRITE (MSGTXT,1015) II, JJ
         CALL MSGWRT (2)
         LAPREC = 2 * NC + 3
         MAXREC = (CY - 16) / LAPREC - 5
         IC = MBUFSZ / (2 * LAPREC)
         MAXREC = MIN (MAXREC, IC)
C                                       Init UV read
         ILENBU = 0
         IF (UISSCR) THEN
            CALL UVINIT ('READ', DLUN, DIND, NVISIN, VO, LLREC,
     *         ILENBU, JBUFSZ, BUFF1, BO, IBIND, IRET)
         ELSE
            CALL SDGET ('INIT', BUFF1, BUFF1(1+NRPARM), IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'INIT'
            GO TO 990
            END IF
         INCNT = 0
         KNPTR = 1
C                                       Read first visibility record
 100     IF (UISSCR) THEN
            CALL UVDISK ('READ', DLUN, DIND, BUFF1, NIO, IBIND, IRET)
         ELSE
            CALL SDGET ('READ', BUFF1, BUFF1(1+NRPARM), IRET)
            NIO = 1
            IF (IRET.LT.0) NIO = 0
            IRET = MAX (IRET, 0)
            IBIND = 1
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ'
            GO TO 990
            END IF
         INPTR = IBIND
         IF (NIO.GT.0) THEN
            DO 120 II = 1,NIO
C                                       Translate into pixels
               IF (DOPROJ) THEN
C                                       Get position offset.
                  XX = BUFF1(INPTR+ILOCU)
                  YY = BUFF1(INPTR+ILOCV)
                  CALL XYPIX (XX, YY, XPIX, YPIX, IRET)
                  IF (IRET.NE.0) THEN
                     PRJERR = PRJERR + 1
                     IF (PRJERR.LE.1) THEN
                        WRITE (MSGTXT,1100) IRET
                        CALL MSGWRT (7)
                        END IF
                     NUMOFF = NUMOFF + 1
                     GO TO 120
                     END IF
C                                       Does not fit
                  IF ((XPIX.LT.0.5+XPARM(1)) .OR.
     *               (XPIX.GT.NX+0.5-XPARM(1)) .OR.
     *               (YPIX.LT.0.5+YPARM(1)) .OR.
     *               (YPIX.GT.NY+0.5-YPARM(1))) THEN
                     NUMOFF = NUMOFF + 1
                     GO TO 120
                     END IF
                  NUMKEP = NUMKEP + 1
C                                       Y in u position
                  BUFFM(KNPTR) = YPIX
C                                       X offset in v position
                  BUFFM(KNPTR+1) = XPIX - NX/2 - 1.
                  BUFFM(KNPTR+2) = 1.0
                  IR = INPTR + 3 * (ICHAN-1) + NRPARM
                  KNPTR = KNPTR + 3
                  DO 105 IC = 1,NC
                     BUFFM(KNPTR) = BUFF1(IR)
                     BUFFM(KNPTR+1) = BUFF1(IR+2)
                     KNPTR = KNPTR + 2
                     IR = IR + 3
 105                 CONTINUE
C                                       Already translated
               ELSE
C                                       Y in u position
                  BUFFM(KNPTR) = BUFF1(INPTR+1)
C                                       X offset in v position
                  BUFFM(KNPTR+1) = BUFF1(INPTR) - NX/2 - 1.
                  BUFFM(KNPTR+2) = BUFF1(INPTR+2)
                  IR = INPTR + 2 * ICHAN + 1
                  KNPTR = KNPTR + 3
                  DO 110 IC = 1,NC
                     BUFFM(KNPTR) = BUFF1(IR)
                     BUFFM(KNPTR+1) = BUFF1(IR+1)
                     KNPTR = KNPTR + 2
                     IR = IR + 2
 110                 CONTINUE
                  END IF
               INPTR = INPTR + LLREC
               INCNT = INCNT + 1
C                                       Load into AP.
               IF (INCNT.GE.MAXREC) THEN
                  ITEMP = KNPTR - 1
                  CALL QWR
                  CALL QPUT (BUFFM, UVPTR, ITEMP, 2)
                  CALL QWAIT
C                                       Do the gridding
                  DO 115 IC = 1,NC
                     VISPTR = UVPTR + 2 * IC + 1
                     WTPTR = VISPTR + 1
                     IF (IWT.EQ.2) THEN
                        CALL QGRD7 (UVPTR, VISPTR, WTPTR, GRID(IC), CY,
     *                     CX, CINC, NO2, M, NX, LAPREC, INCNT)
                     ELSE
                        CALL QGRD6 (UVPTR, VISPTR, WTPTR, GRID(IC), CY,
     *                     CX, CINC, NO2, M, NX, LAPREC, INCNT)
                        END IF
                     CALL QWR
 115                 CONTINUE
                  INCNT = 0
                  KNPTR = 1
                  END IF
 120           CONTINUE
            GO TO 100
            END IF
C                                       Finish last buffer
C                                       Load into AP.
         IF (INCNT.GT.0) THEN
            ITEMP = INCNT * LAPREC
            CALL QWR
            CALL QPUT (BUFFM, UVPTR, ITEMP, 2)
            CALL QWAIT
C                                       Do the gridding
            DO 125 IC = 1,NC
               VISPTR = UVPTR + 2 * IC + 1
               WTPTR = VISPTR + 1
               IF (IWT.EQ.2) THEN
                  CALL QGRD7 (UVPTR, VISPTR, WTPTR, GRID(IC), CY, CX,
     *               CINC, NO2, M, NX, LAPREC, INCNT)
               ELSE
                  CALL QGRD6 (UVPTR, VISPTR, WTPTR, GRID(IC), CY, CX,
     *               CINC, NO2, M, NX, LAPREC, INCNT)
                  END IF
               CALL QWR
 125           CONTINUE
            END IF
C                                       Summary
         IF (DOPROJ) THEN
            CALL SDGET ('CLOS', BUFF1, BUFF1(1+NRPARM), IERR)
            WRITE (MSGTXT,1125) NUMKEP, NUMOFF
            IF (IPASS.EQ.1) CALL MSGWRT (3)
            WRITE (MSGTXT,1126) PRJERR
            IF (PRJERR.GT.0) CALL MSGWRT (7)
            IF (NUMKEP.LE.0.0) IRET = 9
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Done gridding find max cwt
         CALL QWAIT
         DO 135 IC = 1,NC
C                                       Loop over plane
            IDATA = GRID(IC)
            DO 133 IY = 1,NY
C                                       Read out gridded data.
               CALL QGET (BUFF2, IDATA, TOLROW, 2)
               IDATA = IDATA + TOLROW
               CALL QWAIT
C                                       Move data: validity image
               KK = -1
               DO 130 II = 1,NX
                  KK = KK + 2
                  MAXCWT = MAX (MAXCWT ,ABS(BUFF2(KK+1)))
 130              CONTINUE
 133           CONTINUE
 135        CONTINUE
         IF (MAXCWT.LE.0.0) MAXCWT = 1.0
         XNLIM = ABS (RWT) * MAXCWT
C                                       Done gridding, read/write planes
         CALL QWAIT
         DO 200 IC = 1,NC
            DEPTH(1) = ICHAN + IC - 1
            CALL COMOFF (CATMAP(KIDIM), CATMAP(KINAX), DEPTH, BLKOF,
     *         IERR)
            BLKOF = BLKOF + BO
            CALL MINIT ('WRIT', LUNL, INDL, NX, NY, WIN,
     *         OUTBUF(1), OBUFSZ, BLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT'
               GO TO 990
               END IF
C                                       Loop over plane
            IDATA = GRID(IC)
            DO 190 IY = 1,NY
C                                       Write
               CALL MDISK ('WRIT', LUNL, INDL, OUTBUF(1), BIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE'
                  GO TO 990
                  END IF
C                                       Read out gridded data.
               CALL QGET (BUFF2, IDATA, TOLROW, 2)
               IDATA = IDATA + TOLROW
               CALL QWAIT
C                                       Move data: validity image
               JJ = BIND - 1
               KK = -1
               IF (IWT.EQ.2) THEN
                  IF (DOABS) THEN
                     DO 140 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF ((ABS(BUFF2(KK+1)).GE.XNLIM) .AND.
     *                     (BUFF2(KK).GT.0.0)) THEN
                           OUTBUF(JJ) = BUFF2(KK+1) * BUFF2(KK+1)
     *                        / BUFF2(KK)
                           DMAX = MAX (DMAX, OUTBUF(JJ))
                           DMIN = MIN (DMIN, OUTBUF(JJ))
                        ELSE
                           OUTBUF(JJ) = FBLANK
                           WASBLK = .TRUE.
                           END IF
 140                    CONTINUE
                  ELSE
                     DO 145 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF ((BUFF2(KK+1).GE.XNLIM) .AND.
     *                     (BUFF2(KK).GT.0.0)) THEN
                           OUTBUF(JJ) = BUFF2(KK+1) * BUFF2(KK+1)
     *                        / BUFF2(KK)
                           DMAX = MAX (DMAX, OUTBUF(JJ))
                           DMIN = MIN (DMIN, OUTBUF(JJ))
                        ELSE
                           OUTBUF(JJ) = FBLANK
                           WASBLK = .TRUE.
                           END IF
 145                    CONTINUE
                     END IF
C                                       Move data: Convolution
               ELSE IF (IWT.GE.0) THEN
                  IF (DOABS) THEN
                     DO 150 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                           OUTBUF(JJ) = BUFF2(KK+IWT) / MAXCWT
                           DMAX = MAX (DMAX, OUTBUF(JJ))
                           DMIN = MIN (DMIN, OUTBUF(JJ))
                        ELSE
                           OUTBUF(JJ) = FBLANK
                           WASBLK = .TRUE.
                           END IF
 150                    CONTINUE
                  ELSE
                     DO 155 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (BUFF2(KK+1).GE.XNLIM) THEN
                           OUTBUF(JJ) = BUFF2(KK+IWT) / MAXCWT
                           DMAX = MAX (DMAX, OUTBUF(JJ))
                           DMIN = MIN (DMIN, OUTBUF(JJ))
                        ELSE
                           OUTBUF(JJ) = FBLANK
                           WASBLK = .TRUE.
                           END IF
 155                    CONTINUE
                     END IF
C                                       Move data: Interpolation
               ELSE
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
 160                    CONTINUE
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
 165                    CONTINUE
                     END IF
                  END IF
 190           CONTINUE
            CALL MDISK ('FINI', LUNL, INDL, OUTBUF(1), BIND,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FINISH'
               GO TO 990
               END IF
 200        CONTINUE
 300     CONTINUE
C                                       Close files
      CALL QRLSE
      CATMAR(KRDMX) = DMAX
      CATMAR(KRDMN) = DMIN
      IF (WASBLK) THEN
         CATMAR(KRBLK) = FBLANK
      ELSE
         CATMAR(KRBLK) = 0.0
         END IF
      IF (UISSCR) CALL ZCLOSE (DLUN, DIND, IERR)
      CALL ZCLOSE (LUNL, INDL, IERR)
      WRITE (MSGTXT,1315) MAXCWT
      CALL MSGWRT (3)
      IF ((IWT.LT.0) .OR. (IWT.GE.2)) THEN
         WRITE (MSGTXT,1316) XNLIM
      ELSE
         WRITE (MSGTXT,1317) XNLIM
         END IF
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDGRID: ERROR',I4,1X,A,'ING OUTPUT')
 1010 FORMAT ('SDGRID: ERROR',I4,1X,A,'ING UV SCRATCH FILE')
 1015 FORMAT ('Begin gridding channels',I5,' through',I5,' in core')
 1020 FORMAT ('SDGRID:',I8,' TOO FEW AP WORDS AVAILABLE')
 1100 FORMAT ('SDGRID: ERROR',I5,' PROJECTING POSITION')
 1125 FORMAT (I9,' records kept;',I9,' others fell off the grid')
 1126 FORMAT (I9,' ERRORS PROJECTING COORDINATES')
 1315 FORMAT ('Maximum convolved weight =',1PE12.4)
 1316 FORMAT ('   used to scale cutoff to',1PE12.4)
 1317 FORMAT ('   used to scale cutoff to',1PE12.4,
     *   ' and to scale output image')
      END
