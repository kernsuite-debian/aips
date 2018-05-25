      SUBROUTINE AU5A (BRANCH)
C-----------------------------------------------------------------------
C! verbs to load images to the TV including ROAM
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2008-2009, 2011
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
C   AU5A loads images to the TV, does roam operations
C   BRANCH = 1  TVLOD  load image to one plane
C          = 2  TVROAM   load > 1 plane, then roam
C          = 3  SETROAM  reset roam mode, then roam
C          = 4  REROAM   roam with current mode
C          = 5  ROAMOFF  current visible image in roam mode -> non-roam
C                        mode and still visible
C   Roam modes (TVSPLM) : 10 * (# images in x) + (# images in y)
C            (TVSPLC) digits imply channels & their order
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAME*12, CLASS*6, PRGNAM*6, PTYPE*2, CDUM*2
      INTEGER   POTERR, IERR, ICHAN, IVOL, USID, SEQNO, INC(2), DLUN,
     *   DIND, CNO, INBUF(MABFSS), ITVC(4), TVC(4), IWIN(4), WIN(4),
     *   TYPE, ICS(16), ICNOW, IOF, I, INX, INY, IBSIZ, SCROLX, SCROLY,
     *   IDUM, IX, IY, QCHAN(4), QUAD, QTVC(4,4), QIMC(4,4), QVOL(4),
     *   QCNO(4), J, II, ITEMP, ZAND, CATEMP(256), NDIFF
      LOGICAL   F, ODD
      REAL      LBLC(7), LTRC(7), RINBUF(MABFSS), RDUM(2)
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR / RINBUF
      EQUIVALENCE (INBUF, RINBUF)
      DATA F  /.FALSE./
      DATA PRGNAM /'AU5A '/
      DATA DLUN /16/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.5)) GO TO 999
      POTERR = 101
      CALL TVOPEN (INBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IBSIZ = 2 * MABFSS
C                                       Map open junk: TVLOD, TVROAM
      IF (BRANCH.LE.2) THEN
         CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVERB ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         USID = NLUSER
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, NAME)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASS)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVERB ('TXINC', 'I', 1, 0, INC(1), RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVERB ('TYINC', 'I', 1, 0, INC(2), RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         PTYPE = '  '
         CALL MAPOPN ('READ', IVOL, NAME, CLASS, SEQNO, PTYPE, USID,
     *      DLUN, DIND, CNO, CATBLK, INBUF, IERR)
         POTERR = 33
         IF (IERR.GT.1) GO TO 975
         IF ((PTYPE.EQ.'UV') .OR. (CATBLK(KIPCN).GT.0)) THEN
            MSGTXT = 'UV DATA DOES NOT MAKE TV IMAGES'
            CALL MSGWRT (6)
            POTERR = 101
            GO TO 970
            END IF
         CALL ADVERB ('TBLC', 'R', 7, 0, IDUM, LBLC, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         CALL ADVERB ('TTRC', 'R', 7, 0, IDUM, LTRC, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
C                                       Image cat fill in some
         CALL ADVERB ('PIXRANGE', 'R', 2, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         CALL RNGSET (RDUM, CATR(KRDMX), CATR(KRDMN), CATR(IRRAN))
         CATBLK(IIVOL) = IVOL
         CATBLK(IICNO) = CNO
         CALL ADVERB ('FUNCTYPE', 'C', 1, 2, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         CALL CHR2H (2, CDUM, 1, CATH(IITRA))
         CALL ADVERB ('TVCORN', 'I', 2, 0, ITVC, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         POTERR = 49
         END IF
C                                       Branch to verb
      GO TO (100, 200, 300, 400, 500), BRANCH
C-----------------------------------------------------------------------
C                                       TVLOD
C                                       load one image plane
C-----------------------------------------------------------------------
C                                       check TV state
 100  IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         POTERR = 77
         GO TO 970
         END IF
C                                       set windows
      TYPE = -1
      CALL TVWIND (TYPE, INC, LBLC, LTRC, ICHAN, ITVC, IWIN, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       load it
      CALL MOVIST ('OFFF', ICHAN, 0, 0, 0, IERR)
      CALL TVLOAD (DLUN, DIND, ICHAN, INC, ITVC, IWIN, IBSIZ, RINBUF,
     *   IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVROAM
C                                       load > 1 plane, then roam
C-----------------------------------------------------------------------
C                                       set windows
 200  CALL ADVERB ('ROMODE', 'I', 1, 0, TYPE, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
C                                       get LUTs from TVCHAN
      ICNOW = MAX (1, ICHAN)
      I = ICHAN
      INC(1) = MAX (1, INC(1))
      INC(2) = MAX (1, INC(2))
      CALL TVWIND (TYPE, INC, LBLC, LTRC, ICHAN, ITVC, IWIN, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Only 1 plane is needed
      IF (TYPE.LE.0) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (2)
         ICHAN = I
         GO TO 100
         END IF
C                                       Set mode parameter
      TVSPLM = TYPE
C                                       Turn off all channels
      CALL FILL (4, 0, ICS)
      CALL YHOLD ('ONNN', IERR)
      DO 210 I = 1,NGRAY
         CALL YSLECT ('OFFF', I, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 970
 210     CONTINUE
C                                       Off the zoom
      I = MAXXTV(1)/2
      IOF = MAXXTV(2)/2
      CALL YZOOMC (0, I, IOF, F, IERR)
      INX = TYPE / 100
      INY = MOD (TYPE, 100)
C                                       LUTS
      ICNOW = 2 ** (ICNOW-1)
      I = INX * INY
      I = (2 ** I) - 1
      CALL YLUT ('READ', ICNOW, 4, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL YLUT ('WRIT', I, 4, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL YLUT ('READ', ICNOW, 2, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL YLUT ('WRIT', I, 2, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL YLUT ('READ', ICNOW, 1, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL YLUT ('WRIT', I, 1, .FALSE., INBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       load images
      I = 0
      DO 295 IY = 1,INY
C                                       top-most
         IF (IY.EQ.1) THEN
            TVC(4) = ITVC(4) - (INY-1) * MAXXTV(2)
            TVC(2) = 1
            WIN(4) = IWIN(4)
C                                       other
         ELSE
            TVC(4) = MAXXTV(2)
            TVC(2) = ITVC(2)
            IF (IY.NE.INY) TVC(2) = 1
            WIN(4) = IWIN(4) -
     *         (ITVC(4) - (INY + 1 - IY) * MAXXTV(2)) * INC(2)
            END IF
         WIN(2) = WIN(4) - (TVC(4)-TVC(2)) * INC(2)
         IF (WIN(2).LT.IWIN(2)) THEN
            TVC(2) = TVC(2) + (IWIN(2) - WIN(2)) / INC(2)
            WIN(2) = IWIN(2)
            END IF
C                                       X axis
         DO 290 IX = 1,INX
            I = I + 1
C                                       Turn on image during load
            ICS(I) = I
            CALL YSLECT ('ONNN', ICS(I), 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL YZERO (ICS(I), IERR)
            IF (IERR.NE.0) GO TO 970
            ICNOW = 2 ** (I-1)
            SCROLX = 0
            SCROLY = 0
            CALL YSCROL (ICNOW, SCROLX, SCROLY, F, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL YHOLD ('OFFF', IERR)
C                                       right-most
            IF (IX.EQ.1) THEN
               TVC(3) = ITVC(3) - (INX-1) * MAXXTV(1)
               TVC(1) = 1
               WIN(3) = IWIN(3)
C                                       other
            ELSE
               TVC(3) = MAXXTV(1)
               TVC(1) = ITVC(1)
               IF (IX.NE.INX) TVC(1) = 1
               WIN(3) = IWIN(3) -
     *            (ITVC(3) - (INX + 1 - IX) * MAXXTV(1)) * INC(1)
               END IF
            WIN(1) = WIN(3) - (TVC(3)-TVC(1)) * INC(1)
            IF (WIN(1).LT.IWIN(1)) THEN
               TVC(1) = TVC(1) + (IWIN(1) - WIN(1)) / INC(1)
               WIN(1) = IWIN(1)
               END IF
C                                       do load
            CALL TVLOAD (DLUN, DIND, ICS(I), INC, TVC, WIN, IBSIZ,
     *         RINBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL YHOLD ('ONNN', IERR)
            CALL YSLECT ('OFFF', ICS(I), 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 970
 290        CONTINUE
 295     CONTINUE
      GO TO 420
C-----------------------------------------------------------------------
C                                       SETROAM
C                                       set roam parm, then roam
C-----------------------------------------------------------------------
 300  MSGTXT = 'SETROAM NO LONGER DOES ANYTHING BUT REROAM'
      CALL MSGWRT (2)
C     CALL ADVERB ('ROMODE', 'I', 1, 0, TVSPLM, RDUM, CDUM)
C     IF (ERRNUM.NE.0) GO TO 975
C     CALL ADVERB ('TVCHAN', 'I', 1, 0, TVSPLC, RDUM, CDUM)
C     IF (ERRNUM.NE.0) GO TO 975
      GO TO 400
C-----------------------------------------------------------------------
C                                       REROAM
C                                       roam with existing images, parm
C-----------------------------------------------------------------------
 400  IF (TVSPLM.NE.0) GO TO 405
         WRITE (MSGTXT,1400)
         CALL MSGWRT (6)
         POTERR = 101
         GO TO 975
C                                       Convert to mode, channels
 405  TYPE = TVSPLM
      DO 410 I = 1,16
         ICS(I) = I
 410     CONTINUE
C                                       Do the roam
 420  POTERR = 49
      CALL YHOLD ('OFFF', IERR)
      CALL TVROAM (TYPE, ICS, INBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      IF (BRANCH.EQ.2) GO TO 970
      GO TO 975
C-----------------------------------------------------------------------
C                                       ROAMOFF
C                                       replicate current picture in
C                                       simple TV image
C-----------------------------------------------------------------------
C                                       loop through quadrants learning
C                                       about what is there
 500  POTERR = 0
      ODD = .FALSE.
      NDIFF = 0
      DO 515 I = 1,4
         QUAD = 5 - I
         DO 510 J = 1,NGRAY
            ITEMP = 2 ** (J-1)
            IF (ZAND(ITEMP,TVLIMG(QUAD)).NE.0) THEN
               QCHAN(QUAD) = J
               IX = MAXXTV(1) / 2
               IY = MAXXTV(2) / 2
               CALL YCREAD (J, IX, IY, CATBLK, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (6,1500) IERR, J
                  CALL MSGWRT (6)
                  GO TO 975
                  END IF
               CALL COPY (4, CATBLK(IIWIN), QIMC(1,QUAD))
               CALL COPY (4, CATBLK(IICOR), QTVC(1,QUAD))
               QVOL(QUAD) = CATBLK(IIVOL)
               QCNO(QUAD) = CATBLK(IICNO)
               IF (QVOL(QUAD).NE.QVOL(4)) ODD = .TRUE.
               IF (QCNO(QUAD).NE.QCNO(4)) ODD = .TRUE.
               DO 505 II = QUAD+1,4
                  IF (QCHAN(QUAD).NE.QCHAN(II)) NDIFF = NDIFF + 1
 505              CONTINUE
               GO TO 515
               END IF
 510        CONTINUE
 515     CONTINUE
C                                       input TVCHAN
      CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
C                                       not ROAM probably
      IF ((ODD) .OR. ((NDIFF.NE.6) .AND. (NDIFF.NE.4))) THEN
         MSGTXT = 'TV DOES NOT SEEM TO BE IN A ROAM MODE'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1520) 'TV channels ', QCHAN
         CALL MSGWRT (6)
         WRITE (MSGTXT,1520) 'Disk numbers', QVOL
         CALL MSGWRT (6)
         WRITE (MSGTXT,1520) 'Cat numbers ', QCNO
         CALL MSGWRT (6)
         POTERR = 49
         CALL YHOLD ('ONNN', IERR)
         IF (IERR.NE.0) GO TO 975
         DO 525 J = 1,NGRAY
            IF (J.NE.QCHAN(1)) THEN
               CALL YSLECT ('OFFF', J, 0, RINBUF, IERR)
            ELSE
               CALL YSLECT ('ONNN', J, 0, RINBUF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 975
 525        CONTINUE
         ITEMP = (2 ** (NGRAY+1)) - 1
         CALL YSCROL (ITEMP, 0, 0, .TRUE., IERR)
         IF (IERR.NE.0) GO TO 975
         I = MAXXTV(1)/2
         IOF = MAXXTV(2)/2
         CALL YZOOMC (0, I, IOF, F, IERR)
         IF (IERR.NE.0) GO TO 975
         CALL YHOLD ('OFFF', IERR)
         IF (IERR.EQ.0) POTERR = 0
         GO TO 975
C                                       roam instead
      ELSE
         INC(1) = 1
         INC(2) = 1
         IF (QTVC(3,3).NE.QTVC(1,3)) INC(1) = (QIMC(3,3)-QIMC(1,3)) /
     *      (QTVC(3,3)-QTVC(1,3))
         IF (QTVC(4,3).NE.QTVC(2,3)) INC(2) = (QIMC(4,3)-QIMC(2,3)) /
     *      (QTVC(4,3)-QTVC(2,3))
C                                       2x2
         IF (NDIFF.EQ.6) THEN
            IWIN(1) = QIMC(3,3) - (TVSPLT(1) - 2) * INC(1)
            IWIN(2) = QIMC(4,3) - TVSPLT(2) * INC(2)
            IWIN(3) = IWIN(1) + (MAXXTV(1) - 1) * INC(1)
            IWIN(4) = IWIN(2) + (MAXXTV(2) - 1) * INC(2)
C                                       1x2
         ELSE IF (QCHAN(1).EQ.QCHAN(2)) THEN
            IWIN(1) = QIMC(1,1)
            IWIN(3) = QIMC(3,1)
            IWIN(2) = QIMC(4,3) - TVSPLT(2) * INC(2)
            IWIN(4) = IWIN(2) + (MAXXTV(2) - 1) * INC(2)
C                                       2x1
         ELSE
            IWIN(2) = QIMC(2,1)
            IWIN(4) = QIMC(4,1)
            IWIN(1) = QIMC(3,3) - (TVSPLT(1) - 2) * INC(1)
            IWIN(3) = IWIN(1) + (MAXXTV(1) - 1) * INC(1)
            END IF
         IF (IWIN(1).LE.0) THEN
            IWIN(3) = MIN (IWIN(3)-IWIN(1)+1, CATBLK(KINAX))
            IWIN(1) = 1
            END IF
         IF (IWIN(3).GT.CATBLK(KINAX)) THEN
            IWIN(1) = MAX (1, IWIN(1) - IWIN(3) + CATBLK(KINAX))
            IWIN(3) = CATBLK(KINAX)
            END IF
         IF (IWIN(2).LE.0) THEN
            IWIN(4) = MIN (IWIN(4)-IWIN(2)+1, CATBLK(KINAX+1))
            IWIN(2) = 1
            END IF
         IF (IWIN(4).GT.CATBLK(KINAX+1)) THEN
            IWIN(2) = MAX (1, IWIN(2) - IWIN(4) + CATBLK(KINAX+1))
            IWIN(4) = CATBLK(KINAX+1)
            END IF
         IX = (IWIN(3) - IWIN(1)) / INC(1) + 1
         IY = (IWIN(4) - IWIN(2)) / INC(2) + 1
         I = (MAXXTV(1) - IX) / 2
         I = MAX (0, I)
         ITVC(3) = MAXXTV(1) - I
         ITVC(1) = ITVC(3) - IX + 1
         I = (MAXXTV(2) - IY) / 2
         I = MAX (0, I)
         ITVC(4) = MAXXTV(2) - I
         ITVC(2) = ITVC(4) - IY + 1
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
         CALL H2CHR (6, KHIMCO, CATH(KHIMN), CLASS)
         SEQNO = CATBLK(KIIMS)
         PTYPE = '  '
         CALL MAPOPN ('READ', IVOL, NAME, CLASS, SEQNO, PTYPE, USID,
     *      DLUN, DIND, CNO, CATEMP, INBUF, IERR)
         POTERR = 33
         IF (IERR.GT.1) GO TO 975
         CALL YHOLD ('ONNN', IERR)
         IF (IERR.NE.0) GO TO 970
         DO 535 J = 1,NGRAY
            IF (J.NE.QCHAN(1)) THEN
               CALL YSLECT ('OFFF', J, 0, RINBUF, IERR)
            ELSE
               CALL YSLECT ('ONNN', J, 0, RINBUF, IERR)
               IF (IERR.EQ.0) CALL YZERO (J, IERR)
               END IF
            IF (IERR.NE.0) GO TO 970
 535     CONTINUE
         ITEMP = (2 ** (NGRAY+1)) - 1
         CALL YSCROL (ITEMP, 0, 0, .TRUE., IERR)
         IF (IERR.NE.0) GO TO 970
         I = MAXXTV(1)/2
         IOF = MAXXTV(2)/2
         CALL YZOOMC (0, I, IOF, F, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (ICHAN.GT.0) THEN
            CALL ADVRBS ('TVCHAN', 'I', 1, 0, QCHAN(1), RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 975
            END IF
C                                       load it
         ICHAN = QCHAN(1)
         POTERR = 49
         CALL MOVIST ('OFFF', ICHAN, 0, 0, 0, IERR)
         CALL TVLOAD (DLUN, DIND, ICHAN, INC, ITVC, IWIN, IBSIZ, RINBUF,
     *      IERR)
         IF (IERR.EQ.0) POTERR = 0
         CALL YHOLD ('OFFF', IERR)
         WRITE (MSGTXT,1530) ICHAN, IWIN
         CALL MSGWRT (3)
         END IF
C-----------------------------------------------------------------------
C                                       Close down ops
 970  CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F, INBUF,
     *   IERR)
C
 975  CALL YHOLD ('OFFF', IERR)
      CALL TVCLOS (INBUF, IERR)
C                                       POPS error
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('LOADING ONLY ONE PLANE: NO NEED FOR ROAM')
 1400 FORMAT ('SPLIT CONDITION UNKNOWN: USE TVROAM OR SETROAM')
 1500 FORMAT ('ERROR',I5,' GETTING IMAGE CATALOG FOR CHANNEL',I3)
 1520 FORMAT ('MISMATCHED ',A,4I7)
 1530 FORMAT ('Load image channel',I3,' tblc=',2I6,' ttrc=',2I6)
      END
