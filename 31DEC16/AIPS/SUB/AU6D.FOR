      SUBROUTINE AU6D (BRANCH)
C-----------------------------------------------------------------------
C! verbs to do image statistics in blotch regions: TVSTAT, IMSTAT
C# POPS-appl TV-appl Map-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2005, 2007-2010, 2012-2014, 2016
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
C   AU6D performs the function of obtaining the image rms and mean
C   with an option to set blotch areas on the TV first
C   BRANCH = 1 : TVSTAT   TV blotch, then get statistics
C            2 : IMSTAT   no TV function, just statistics
C   Adverbs: output (TVSTAT and IMSTAT)
C      PIXRMS   :  rms over specified area
C      PIXAVG   :  mean over specified area
C   Inputs:  (IMSTAT only - usual INNAME et al, BLC, TRC)
C      The name and corners for TVSTAT are taken from the TV
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   MPOLY
      PARAMETER (MPOLY=50)
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER STRING*20, PRGNAM*6, LLCHAR(5)*4, XXCHAR*4, BUN2*4,
     *   MMCHAR(5)*4, SNAME*12, SCLAS*6, STYPE*2, BUNITS*8, CDUM*1
      INTEGER   POTERR, IERR, NPOLY, BUF(MABFSS), USID, IVOL, SEQNO,
     *   DLUN, DIND, CNO, NX, NY, I, J, WIN(4), IOFF, IG, NVERT(MPOLY),
     *   XV(10*MPOLY), YV(10*MPOLY), MV, I1U, I2U, I3U, I4U, I5U, I6U,
     *   I7U, I1L, I2L, I3L, I4L, I5L, I6L, I7L, IX, IY, I3, I4, I5, I6,
     *   I7, JJ, DEPTH(5), BLKOF, LNX, IXL(MPOLY), IXU(MPOLY), IBP,
     *   ITEMP, MAXDEP(7), MINDEP(7), ICH, ILEN, IBUFSZ, IDUM, IROUND,
     *   JTRIM
      LOGICAL   T, F, DOIT, ADOIT, DOINV, CIRCLE, XDOIT, TVOPN, MAOPN
      REAL      LBLC(7), LTRC(7), RBUF(MABFSS), X, Y, AREAI, AREA, RDUM,
     *   XC, YC, RC, RX1, RX2
      DOUBLE PRECISION S, S2, Z, ZMAX, SKYPOS(3), ZMIN, NZ, SW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ RBUF
      EQUIVALENCE (BUF, RBUF)
      DATA PRGNAM /'AU6D  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA DLUN /16/
      DATA LLCHAR, MMCHAR /'LL  ','RA  ','RA--','GLON','ELON',
     *   'MM  ','DEC ','DEC-','GLAT','ELAT'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.2)) GO TO 999
      IBUFSZ = 2 * MABFSS
      CALL ADVERB ('DOINVERS', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      DOINV = RDUM.GT.0.0
      TVOPN = F
      MAOPN = F
C
      GO TO (100, 200), BRANCH
C-----------------------------------------------------------------------
C                                       TVSTAT
C-----------------------------------------------------------------------
 100  CALL TVOPEN (BUF, IERR)
      IF (IERR.NE.0) GO TO 980
      TVOPN = T
      IG = MIN (3, NGRAPH)
C                                       check TV state
      IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         POTERR = 77
         GO TO 900
         END IF
C                                       do interactive polygons
      NPOLY = 0
      CALL GRPOLY (MPOLY, IG, NPOLY, NVERT, XV, YV, BUF, IERR)
      IF ((IERR.NE.0) .OR. (NPOLY.LE.0)) THEN
         POTERR = 49
         IF (IERR.EQ.0) THEN
            POTERR = 101
            MSGTXT = 'NO BLOTCH REGIONS WERE SPECIFIED'
            CALL MSGWRT (8)
            END IF
         GO TO 900
         END IF
C                                       close TV
      CALL TVCLOS (BUF, IERR)
      IF (IERR.EQ.0) TVOPN = F
      I3L = MAX (1, CATBLK(IIDEP))
      I4L = MAX (1, CATBLK(IIDEP+1))
      I5L = MAX (1, CATBLK(IIDEP+2))
      I6L = MAX (1, CATBLK(IIDEP+3))
      I7L = MAX (1, CATBLK(IIDEP+4))
      CALL COPY (4, CATBLK(IIWIN), WIN)
      USID = CATBLK(KIIMU)
      IVOL = CATBLK(IIVOL)
      SEQNO = CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), SNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), SCLAS)
      STYPE = 'MA'
      POTERR = 33
      CALL MAPOPN ('READ', IVOL, SNAME, SCLAS, SEQNO, STYPE, USID, DLUN,
     *    DIND, CNO, CATBLK, BUF, IERR)
      IF (IERR.GT.1) GO TO 900
      MAOPN = T
      IF (IERR.EQ.1) GO TO 900
C                                       number of vertices
      MV = 0
      DO 115 I = 1,NPOLY
         MV = MV + NVERT(I)
 115     CONTINUE
      I1U = 1
      I2U = 1
      I1L = 2048
      I2L = 2048
      DO 120 I = 1,MV
         I1L = MIN (I1L, XV(I))
         I1U = MAX (I1U, XV(I))
         I2L = MIN (I2L, YV(I))
         I2U = MAX (I2U, YV(I))
 120     CONTINUE
      I3U = I3L
      I4U = I4L
      I5U = I5L
      I6U = I6L
      I7U = I7L
      IF (DOINV) THEN
         I1L = WIN(1)
         I2L = WIN(2)
         I1U = WIN(3)
         I2U = WIN(4)
         END IF
      GO TO 220
C-----------------------------------------------------------------------
C                                       IMSTAT
C                                       from disk only
C-----------------------------------------------------------------------
C                                       Open the map
 200  CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      USID = NLUSER
      STYPE = 'MA'
      POTERR = 33
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, SNAME)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, SCLAS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL MAPOPN ('READ', IVOL, SNAME, SCLAS, SEQNO, STYPE, USID, DLUN,
     *    DIND, CNO, CATBLK, BUF, IERR)
      IF (IERR.GT.1) GO TO 980
      MAOPN = T
      IF (IERR.EQ.1) GO TO 900
C                                       Basic window
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, LBLC, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      CALL ADVERB ('TRC', 'R', 7, 0, IDUM, LTRC, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
C                                       circular?
      JJ = IROUND (LBLC(1))
      CIRCLE = JJ.EQ.-1
      IF (CIRCLE) THEN
         RC = LBLC(2)
         XC = LTRC(1)
         YC = LTRC(2)
         LBLC(1) = XC - RC
         LTRC(1) = XC + RC
         LBLC(2) = YC - RC
         LTRC(2) = YC + RC
         END IF
      POTERR = 101
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), LBLC, LTRC, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       loop limits
      IF (DOINV) THEN
         I7L = 1
         I6L = 1
         I5L = 1
         I4L = 1
         I3L = 1
         I2L = 1
         I1L = 1
         I7U = MAX (1, CATBLK(KINAX+6))
         I6U = MAX (1, CATBLK(KINAX+5))
         I5U = MAX (1, CATBLK(KINAX+4))
         I4U = MAX (1, CATBLK(KINAX+3))
         I3U = MAX (1, CATBLK(KINAX+2))
         I2U = MAX (1, CATBLK(KINAX+1))
         I1U = MAX (1, CATBLK(KINAX))
      ELSE
         I7L = LBLC(7) + 0.01
         I6L = LBLC(6) + 0.01
         I5L = LBLC(5) + 0.01
         I4L = LBLC(4) + 0.01
         I3L = LBLC(3) + 0.01
         I2L = LBLC(2) + 0.01
         I1L = LBLC(1) + 0.01
         I7U = LTRC(7) + 0.01
         I6U = LTRC(6) + 0.01
         I5U = LTRC(5) + 0.01
         I4U = LTRC(4) + 0.01
         I3U = LTRC(3) + 0.01
         I2U = LTRC(2) + 0.01
         I1U = LTRC(1) + 0.01
         END IF
C                                       SO DO IT
 220  S = 0.0D0
      S2 = 0.0D0
      SW = 0.0D0
      NZ = 0.0D0
      ZMAX = -1.0D15
      ZMIN = 1.0D15
      WIN(1) = I1L
      WIN(2) = I2L
      WIN(3) = I1U
      WIN(4) = I2U
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      POTERR = 50
      DO 295 I7 = I7L,I7U
      DO 295 I6 = I6L,I6U
      DO 295 I5 = I5L,I5U
      DO 295 I4 = I4L,I4U
      DO 295 I3 = I3L,I3U
         DOIT = ((I7.LT.LBLC(7)) .OR. (I7.GT.LTRC(7))
     *      .OR. (I6.LT.LBLC(6)) .OR. (I6.GT.LTRC(6))
     *      .OR. (I5.LT.LBLC(5)) .OR. (I5.GT.LTRC(5))
     *      .OR. (I4.LT.LBLC(4)) .OR. (I4.GT.LTRC(4))
     *      .OR. (I3.LT.LBLC(3)) .OR. (I3.GT.LTRC(3)))
     *      .AND. (BRANCH.EQ.2)
         DEPTH(1) = I3
         DEPTH(2) = I4
         DEPTH(3) = I5
         DEPTH(4) = I6
         DEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH, BLKOF, IERR)
         BLKOF = BLKOF + 1
C                                       Initialize for read:
         CALL MINIT ('READ', DLUN, DIND, NX, NY, WIN, BUF, IBUFSZ,
     *      BLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR
            CALL MSGWRT (8)
            GO TO 900
            END IF
         DO 290 IY = I2L,I2U
            CALL MDISK ('READ', DLUN, DIND, BUF, IBP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1225) IERR
               CALL MSGWRT (8)
               GO TO 900
               END IF
C                                       set interior segment list
            LNX = 1
            IXL(1) = I1L
            IXU(1) = I1U
            ADOIT = .FALSE.
            IF (BRANCH.EQ.2) THEN
               IF (DOINV) THEN
                  ADOIT = (DOIT) .OR. (IY.LT.LBLC(2)) .OR.
     *               (IY.GT.LTRC(2))
                  END IF
               IF (CIRCLE) THEN
                  RDUM = RC*RC - (IY-YC)**2
                  RDUM = SQRT (MAX(0.0, RDUM))
                  RX1 = XC - RDUM
                  RX2 = XC + RDUM
               ELSE
                  RX1 = LBLC(1)
                  RX2 = LTRC(1)
                  END IF
               END IF
C                                       loop over polygons
            IF (BRANCH.NE.2) THEN
               CALL BLTLIS (1, 1, NPOLY, NVERT, XV, YV, IY, LNX, IXL,
     *            IXU)
               IF ((LNX.LE.0) .AND. (.NOT.DOINV)) GO TO 290
               END IF
C                                       loop over row
            IOFF = IBP - I1L
            DO 285 IX = I1L,I1U
               IF (RBUF(IOFF+IX).NE.FBLANK) THEN
C                                       Inside a blotch?
                  XDOIT = ADOIT
                  IF (.NOT.XDOIT) THEN
                     IF (BRANCH.EQ.2) THEN
                        IF (DOINV) THEN
                           XDOIT = (IX.LT.RX1) .OR. (IX.GT.RX2)
                        ELSE
                           XDOIT = (IX.GE.RX1) .AND. (IX.LE.RX2)
                           END IF
                     ELSE
                        DO 275 I = 1,LNX
                           IF ((IX.GE.IXL(I)). AND. (IX.LE.IXU(I)))
     *                        XDOIT = .TRUE.
 275                       CONTINUE
C                                       in blotch
                        IF (XDOIT) THEN
                           XDOIT = .NOT.DOINV
C                                       not in blotch
                        ELSE
                           XDOIT = DOINV
                           END IF
                        END IF
                     END IF
C                                       Add it in
                  IF (XDOIT) THEN
                     IF (RBUF(IOFF+IX).NE.0.0) THEN
                        Z = RBUF(IOFF+IX)
                        S = S + Z
                        S2 = S2 + Z * Z
                        SW = SW + 1.0D0
                        IF (Z.GT.ZMAX) THEN
                           ZMAX = Z
                           CALL COPY (5, DEPTH, MAXDEP(3))
                           MAXDEP(1) = IX
                           MAXDEP(2) = IY
                           END IF
                        IF (Z.LT.ZMIN) THEN
                           ZMIN = Z
                           CALL COPY (5, DEPTH, MINDEP(3))
                           MINDEP(1) = IX
                           MINDEP(2) = IY
                           END IF
                    ELSE
                       NZ = NZ + 1.0D0
                       END IF
                     END IF
                  END IF
 285           CONTINUE
 290        CONTINUE
 295     CONTINUE
C                                       no data
      POTERR = 24
      IF (SW.LE.0) THEN
         WRITE (MSGTXT,1295)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Print results
      S = S / SW
      S2 = S2 / SW - S * S
      IF (S2.LT.0.0D0) S2 = 0.0D0
      S2 = SQRT (S2)
      RDUM = S
      CALL ADVRBS ('PIXAVG', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      RDUM = S2
      CALL ADVRBS ('PIXSTD', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      RDUM = ZMAX
      CALL ADVRBS ('PIXVAL', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      RDUM = ZMIN
      CALL ADVRBS ('PIX2VAL', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      CALL ADVRBS ('PIXXY', 'I', 7, 0, MAXDEP, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      CALL ADVRBS ('PIX2XY', 'I', 7, 0, MINDEP, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      CALL H2CHR (8, 1, CATH(KHBUN), BUNITS)
      WRITE (MSGTXT,1300) S, S2, BUNITS, SW
      CALL MSGWRT (5)
      WRITE (MSGTXT,1304) NZ
      IF (NZ.GT.0) CALL MSGWRT (5)
      WRITE (MSGTXT,1301) ZMAX, MAXDEP
      CALL MSGWRT (5)
      LOCNUM = 1
      CALL SETLOC (MAXDEP(3), .FALSE.)
      X = MAXDEP(1)
      Y = MAXDEP(2)
      CALL XYVAL (X, Y, SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1305) IERR
         CALL MSGWRT (7)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         WRITE (MSGTXT,1302)
         ICH = 9
         DO 310 I = 1,2
            ITEMP = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), ITEMP, ILEN, STRING)
            MSGTXT(ICH:) = STRING(:ILEN)
            ICH = ICH + ILEN + 2
 310        CONTINUE
         CALL MSGWRT (5)
         END IF
C                                       Secondary axes
      IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
         WRITE (MSGTXT,1302)
         ICH = 9
         DO 315 I = 1,2
            IF (NCHLAB(I,LOCNUM).GT.0) THEN
               MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
               ICH = ICH + NCHLAB(I,LOCNUM) + 2
               END IF
 315        CONTINUE
         CALL MSGWRT (5)
         END IF
C                                       Minimum
 320  WRITE (MSGTXT,1320) ZMIN, MINDEP
      CALL MSGWRT (5)
      CALL SETLOC (MINDEP(3), .FALSE.)
      X = MINDEP(1)
      Y = MINDEP(2)
      CALL XYVAL (X, Y, SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1305) IERR
         CALL MSGWRT (7)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         WRITE (MSGTXT,1302)
         ICH = 9
         DO 330 I = 1,2
            ITEMP = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), ITEMP, ILEN, STRING)
            MSGTXT(ICH:) = STRING(:ILEN)
            ICH = ICH + ILEN + 2
 330        CONTINUE
         CALL MSGWRT (5)
         END IF
C                                       Secondary axes
      IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
         WRITE (MSGTXT,1302)
         ICH = 9
         DO 335 I = 1,2
            IF (NCHLAB(I,LOCNUM).GT.0) THEN
               MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
               ICH = ICH + NCHLAB(I,LOCNUM) + 2
               END IF
 335        CONTINUE
         CALL MSGWRT (5)
         END IF
C                                       total flux ?
 340  CALL CHLTOU (8, BUNITS)
      BUN2 = BUNITS(5:8)
      IF (BUNITS(:4).EQ.'JY/B') THEN
         IF (BUN2.EQ.'EAM') BUN2 = ' '
         AREA = 1.1331 * CATR(KRBMJ) * CATR(KRBMN)
         IF (AREA.GT.0.0) THEN
            J = CATBLK(KIDIM)
            I1U = 0
            I2U = 0
            DO 345 I = 1,J
               CALL H2CHR (4, 1, CATH(KHCTP+2*(I-1)), XXCHAR)
               DO 344 JJ = 1,5
                  IF (XXCHAR.EQ.LLCHAR(JJ)) I1U = I
                  IF (XXCHAR.EQ.MMCHAR(JJ)) I2U = I
 344              CONTINUE
 345           CONTINUE
            IF ((I1U.GT.0) .AND. (I2U.GT.0)) THEN
               AREAI = ABS (CATR(KRCIC+I1U-1) * CATR(KRCIC+I2U-1))
               IF (AREAI.GT.0.0) THEN
                  AREA = AREA / AREAI
                  S = S * SW / AREA
                  I = MAX (1, JTRIM(BUN2))
                  WRITE (MSGTXT,1345) S, BUN2(:I), AREA
                  CALL MSGWRT (5)
                  END IF
               END IF
            END IF
      ELSE IF (BUNITS(:4).EQ.'JY/P') THEN
         IF (BUN2.EQ.'IXEL') BUN2 = ' '
         I = MAX (1, JTRIM(BUN2))
         S = S * SW
         WRITE (MSGTXT,1346) S, BUN2(:I)
         CALL MSGWRT (5)
         END IF
 350  POTERR = 0
C-----------------------------------------------------------------------
C                                       Close down
 900  IF (TVOPN) CALL TVCLOS (BUF, IERR)
      IF (MAOPN) CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F,
     *   BUF, IERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1220 FORMAT ('MAP I/O INITIALIZATION ERROR',I5)
 1225 FORMAT ('MAP READ ERROR',I5)
 1295 FORMAT ('NO VALID PIXELS FOUND')
 1300 FORMAT ('Mean=',1PE10.3,' rms=',1PE10.3,1X,A8,' over',0PF13.0,
     *   ' pixels')
 1301 FORMAT ('Maximum=',1PE11.4,' at',2I7,I6,4I5)
 1302 FORMAT ('Skypos: ')
 1304 FORMAT ('Ignored',F13.0,' pure zero pixels')
 1305 FORMAT ('ERROR',I3,' FINDING SKY COORDINATES')
 1320 FORMAT ('Minimum=',1PE11.4,' at',2I7,I6,4I5)
 1345 FORMAT ('Flux density =',1PE12.4,' Jy',A,'  Beam area =',0PF7.2,
     *   ' pixels')
 1346 FORMAT ('Flux density =',1PE12.4,' Jy',A)
      END
