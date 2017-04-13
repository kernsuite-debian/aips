      SUBROUTINE AU9 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to fit or interpolate the image intensity (MAXFIT, IMVAL)
C# POPS-appl Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003-2006, 2009, 2012-2014
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
C   BRANCH = 1 => MAXFIT   locate and fit a source peak & position
C                            Just a 2d order interpolation to
C                            determine position and strength of maxima
C   BRANCH = 2 => IMVAL    Do cubic interpolation at location PIXXY
C                            of map pixel value
C   BRANCH = 3 => QIMVAL   As IMVAL, but without messages.
C   BRANCH = 4 => IMDIST   Print distance between two pixels in one or
C                          two images.
C   BRANCH = 5 => IMCENTER Find sub-image centroid
C   Inputs (verbs):
C      PIXXY(7)   R    Initial position guess in PIX pixels
C      BLC/TRC    R(*) Sub-image section
C   Outputs (adverbs)
C      PIXXY(7)   R    Fit PIX position (BRANCH=1)
C      PIXVAL     R    Fit PIX pixel value(BRANCH=1)
C                      Interpolated pixel value (BRANCH=2,3)
C      COORDINA(6) R   Image coordinates
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAME*12, CLASS*6, PRGNAM*6, CTEMP*20, PREFIX*5, PTYPE*2,
     *   FTYP*7, CDUM*1, NAME2*12, CLASS2*6, AXL(4)*20
      LOGICAL   F, PFLAG, DOCIRC
      INTEGER   IVOL, USID, DLUN, DIND, CNO, IERR, JERR, INX, SEQNO, IY,
     *   OFBLK, POTERR, I, IDEPTH(5), NAX, ILEN, ICH, BUFF(MABFSS), IX,
     *   NX, NY, ININD, IX1(2), ISGN, WIN(4), I2TMP1, IPLEV, IDUM,
     *   IROUND, JBUFSZ
      REAL      S, DX(2), RBUFF(MABFSS), ARRAY(16,16), RDUM, PIXX(7),
     *   XCOORD(6), FLUX, BLC(7), TRC(7), RADIUS, XSUM, YSUM, FSUM, R,
     *   DOCENT, FSHIFT(2), XYSH(2), DOINV
      DOUBLE PRECISION SKYPOS(3), RA1, RA2, DEC1, DEC2, CT, ST, DD
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /AIPSCR/ RBUFF
      EQUIVALENCE  (BUFF, RBUFF)
      DATA PRGNAM /'AU9 '/
      DATA F /.FALSE./
      DATA DLUN, INX /16, 11/
      DATA ISGN /1/
C-----------------------------------------------------------------------
      IF ((BRANCH.LE.0) .OR. (BRANCH.GT.5)) GO TO 999
      JBUFSZ = 2 * MABFSS
C                                       interpret adverbs
      CALL ADVERB ('PRTLEV', 'I', 1, 0, IPLEV, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOINVERS', 'R', 1, 0, I, DOINV, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (BRANCH.NE.1) DOINV = -1.0
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      USID = NLUSER
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, NAME)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASS)
      IF (ERRNUM.NE.0) GO TO 980
      IF (BRANCH.NE.5) THEN
         CALL ADVERB ('IMSIZE', 'I', 2, 0, IX1, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         NX = IX1(1)
         NY = IX1(2)
         IF ((BRANCH.GE.2) .OR. (NX.LT.3) .OR. (NX.GT.16)) NX = INX
         IF ((BRANCH.GE.2) .OR. (NY.LT.3) .OR. (NY.GT.16)) NY = INX
      ELSE
         CALL ADVERB ('BLC', 'R', 7, 0, IDUM, BLC, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('TRC', 'R', 7, 0, IDUM, TRC, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('FLUX', 'R', 1, 0, IDUM, FLUX, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C                                       open map file
      PTYPE = 'MA'
      CALL MAPOPN ('READ', IVOL, NAME, CLASS, SEQNO, PTYPE, USID, DLUN,
     *    DIND, CNO, CATBLK, BUFF, JERR)
      POTERR = 33
      IF (JERR.GT.1) GO TO 980
C                                       find pixel coords
      NAX = CATBLK(KIDIM)
      IF (BRANCH.EQ.5) THEN
         DOCIRC = BLC(1).LT.-0.5
         RADIUS = BLC(2)
         IF (DOCIRC) THEN
            BLC(1) = 1.0
            BLC(2) = 1.0
            END IF
         CALL WINDOW (NAX, CATBLK(KINAX), BLC, TRC, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL RCOPY (7, BLC, PIXX)
         IF (DOCIRC) THEN
            BLC(1) = -1.0
            BLC(2) = RADIUS
            END IF
      ELSE
         CALL ADVERB ('PIXXY', 'R', 7, 0, IDUM, PIXX, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         IF ((PIXX(1).LE.0.5) .OR. (PIXX(1).GE.CATBLK(KINAX)+0.5) .OR.
     *      (PIXX(2).LE.0.5) .OR. (PIXX(2).GE.CATBLK(KINAX+1)+0.5)) THEN
            POTERR = 101
            WRITE (MSGTXT,1100) PIXX
            CALL MSGWRT (8)
            GO TO 970
            END IF
         END IF
      IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3)) THEN
         CALL ADVERB ('DOCENTER', 'R', 1, 0, IDUM, DOCENT, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
      ELSE
         DOCENT = -1.0
         END IF
      DO 10 I = 1,5
         IDEPTH(I) = 1
         IF (I+2.LE.NAX) THEN
            IDEPTH(I) = PIXX(I+2) + 0.01
            IDEPTH(I) = MAX (1, MIN (CATBLK(KINAX+I+1), IDEPTH(I)))
            END IF
 10      CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH, .FALSE.)
C
      GO TO (100, 100, 100, 200, 300), BRANCH
C-----------------------------------------------------------------------
C                                       MAXFIT
C                                       fit peak * position 1 source
C                                       IMVAL
C                                       Interpolate pixel value
C                                       at position PIXXY.
C-----------------------------------------------------------------------
C                                       read map into array
 100  IF (DOCENT.GT.0.0) THEN
         NX = 1
         NY = 1
         END IF
      WIN(1) = PIXX(1) - 0.5 * NX + 1.
      WIN(1) = MAX (1, WIN(1))
      WIN(2) = PIXX(2) - 0.5 * NY + 1.
      WIN(2) = MAX (1, WIN(2))
      WIN(3) = WIN(1) + NX - 1
      WIN(3) = MIN (WIN(3), CATBLK(KINAX))
      WIN(4) = WIN(2) + NY - 1
      WIN(4) = MIN (WIN(4), CATBLK(KINAX+1))
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
      POTERR = 50
      CALL COMOFF (NAX, CATBLK(KINAX), IDEPTH, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970
      OFBLK = OFBLK + 1
      CALL MINIT ('READ', DLUN, DIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   WIN, BUFF, JBUFSZ, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970
      I = 1
      IF (DOINV.GT.0.0) I = -1
      DO 120 IY = 1,NY
         CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, JERR)
         IF (JERR.NE.0) GO TO 970
         DO 110 IX = 1,NX
            ARRAY(IX,IY) = I * RBUFF(IX+ININD-1)
 110        CONTINUE
 120     CONTINUE
C                                       Set DX for BRANCH = 2,3
      DX(1) = PIXX(1) + 1.0 - WIN(1)
      DX(2) = PIXX(2) + 1.0 - WIN(2)
      IX1(1) = 0
      IX1(2) = 0
      JERR = 0
C                                       find maximum in array
      IF (BRANCH.EQ.1) THEN
         CALL FMAX (ARRAY, NX, NY, ISGN, IX1, S)
C                                       fit parabola
         CALL PFIT (ARRAY, IX1, S, DX, JERR)
         IF (DOINV.GT.0.0) S = -S
         IF (AXTYP(LOCNUM).EQ.1) THEN
            FSHIFT(1) = (DX(1) - IROUND (DX(1))) * AXINC(1,LOCNUM)*3600.
            FSHIFT(2) = (DX(2) - IROUND (DX(2))) * AXINC(2,LOCNUM)*3600.
            DEC1 = RPVAL(3-CORTYP(LOCNUM),LOCNUM)
            XYSH(1) = CATR(KRXSH) * COS(DG2RAD*DEC1) * 3600.
            XYSH(2) = CATR(KRYSH) * 3600.
            IF (ABS(ROT(LOCNUM)).GT.1.E-3) THEN
               R = ROT(LOCNUM) * DG2RAD
               RDUM = XYSH(1)
               XYSH(1) = RDUM * COS(R) + XYSH(2) * SIN(R)
               XYSH(2) = XYSH(2) * COS(R) - RDUM * SIN(R)
               END IF
            FSHIFT(1) = XYSH(1) + FSHIFT(1)
            FSHIFT(2) = XYSH(2) + FSHIFT(2)
            END IF
C                                       Nearest BRANCH=2,3
      ELSE IF (DOCENT.GT.0.0) THEN
         S = ARRAY(1,1)
         IF (S.EQ.FBLANK) JERR = 2
         DX(1) = 1.0
         DX(2) = 1.0
C                                       Interpolate BRANCH=2,3
      ELSE
         CALL CUBINT (ARRAY, DX, NX, NY, S, JERR)
         END IF
      IF (JERR.NE.0) GO TO 960
      CALL ADVRBS ('PIXVAL', 'R', 1, 0, IDUM, S, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
C                                       convert to useful units
      DO 130 I = 1,2
         DX(I) = DX(I) + WIN(I) + IX1(I) - 1.0
         IF ((DX(I).LT.WIN(I)-1) .OR. (DX(I).GT.WIN(I+2)+1))
     *      GO TO 960
 130     CONTINUE
      IF (BRANCH.EQ.1) THEN
         PIXX(1) = DX(1)
         PIXX(2) = DX(2)
         CALL ADVRBS ('PIXXY', 'R', 7, 0, IDUM, PIXX, CDUM)
         IF (ERRNUM.NE.0) GO TO 970
         IF (AXTYP(LOCNUM).EQ.1) THEN
            CALL ADVRBS ('FSHIFT', 'R', 2, 0, IDUM, FSHIFT, CDUM)
            IF (ERRNUM.NE.0) GO TO 970
            END IF
         END IF
C                                       no error
      POTERR = 0
      RDUM = -1.0
      CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       do coordinates
      CALL XYVAL (DX(1), DX(2), SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1132) IERR
         CALL MSGWRT (6)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         END IF
C                                       print results
C                                       quit now for silent
      IF ((BRANCH.NE.3) .AND. ((BRANCH.NE.1) .OR. (IPLEV.GT.-1))) THEN
         WRITE (MSGTXT,1130) DX
         CALL MSGWRT (5)
         IF (IERR.NE.0) GO TO 136
         MSGTXT = 'Skypos: '
         ICH = 9
         DO 135 I = 1,2
            I2TMP1 = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I2TMP1, ILEN, CTEMP)
            MSGTXT(ICH:) = CTEMP(:ILEN)
            ICH = ICH + ILEN + 2
 135        CONTINUE
         CALL MSGWRT (5)
C                                       Secondary axes
 136     IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
            MSGTXT = 'Skypos: '
            ICH = 9
            DO 140 I = 1,2
               IF (NCHLAB(I,LOCNUM).GT.0) THEN
                  MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
                  ICH = ICH + NCHLAB(I,LOCNUM) + 2
                  END IF
 140           CONTINUE
            CALL MSGWRT (5)
            END IF
         POTERR = 0
         CALL METSCA (S, PREFIX, PFLAG)
         CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
         I = 7
         IF (BRANCH.EQ.2) THEN
            FTYP = 'Image'
            I = 5
         ELSE IF (DOINV.GT.0.0) THEN
            FTYP = 'Minimum'
         ELSE
            FTYP = 'Maximum'
            END IF
         IF (PFLAG) THEN
            WRITE (MSGTXT,1145) FTYP(:I), S, CTEMP(1:8)
         ELSE
            WRITE (MSGTXT,1146) FTYP(:I), S, PREFIX, CTEMP(1:8)
            END IF
         CALL MSGWRT (5)
         IF ((BRANCH.EQ.1) .AND. (AXTYP(LOCNUM).EQ.1)) THEN
            WRITE (MSGTXT,1147) FSHIFT
            CALL MSGWRT (5)
            END IF
         END IF
C                                       output COORDINA adverb
      DO 150 I = 1,2
         IF ((CTYP(I,LOCNUM)(:4).EQ.'RA  ') .OR.
     *      (CTYP(I,LOCNUM)(:4).EQ.'RA--')) SKYPOS(I) = SKYPOS(I)/15.0D0
         PFLAG = SKYPOS(I).LT.0.0D0
         SKYPOS(I) = ABS (SKYPOS(I))
         IX = SKYPOS(I)
         XCOORD(3*I-2) = IX
         SKYPOS(I) = (SKYPOS(I) - IX) * 60.0D0
         IX = SKYPOS(I)
         XCOORD(3*I-1) = IX
         XCOORD(3*I) = (SKYPOS(I) - IX) * 60.0D0
         IF (PFLAG) THEN
            XCOORD(3*I-2) = -XCOORD(3*I-2)
            XCOORD(3*I-1) = -XCOORD(3*I-1)
            XCOORD(3*I) = -XCOORD(3*I)
            END IF
 150     CONTINUE
      CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      GO TO 970
C-----------------------------------------------------------------------
C                                       IMDIST
C                                       distance between two points
C-----------------------------------------------------------------------
C                                       all set up for first image
 200  IF (AXTYP(LOCNUM).EQ.0) THEN
         MSGTXT = 'IMDIST WORKS ONLY ON SKY COORDINATE PAIRS'
         CALL MSGWRT (7)
         POTERR = 101
         GO TO 970
         END IF
      CALL XYVAL (PIXX(1), PIXX(2), SKYPOS(1), SKYPOS(2), SKYPOS(3),
     *   IERR)
      IX = MOD (CORTYP(LOCNUM)-1, 2) + 1
      IF (CORTYP(LOCNUM).GT.3) IX = IX + 1
      IY = 3 - IX
      IY = IY + (CORTYP(LOCNUM)-1) / 2
      RA1 = SKYPOS(IX) * DG2RAD
      DEC1 = SKYPOS(IY) * DG2RAD
      I2TMP1 = IX - 1
      CALL AXSTRN (CTYP(IX,LOCNUM), SKYPOS(IX), I2TMP1, ILEN, AXL(1))
      I2TMP1 = IY - 1
      CALL AXSTRN (CTYP(IY,LOCNUM), SKYPOS(IY), I2TMP1, ILEN, AXL(2))
C                                       second image
      CALL ADVERB ('IN2NAME', 'C', 1, 12, IDUM, RDUM, NAME2)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('IN2CLASS', 'C', 1, 6, IDUM, RDUM, CLASS2)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('IN2DISK', 'I', 1, 0, IX, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('IN2SEQ', 'I', 1, 0, IY, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((NAME2.EQ.' ') .OR. (CLASS2.EQ.' ') .OR. (IY.LE.0)) THEN
         MSGTXT = 'Using the same image for both'
         CALL MSGWRT (2)
      ELSE IF ((NAME2.NE.NAME) .OR. (CLASS2.NE.CLASS) .OR. (IX.NE.IVOL)
     *   .OR. (IY.NE.SEQNO)) THEN
         CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F, BUFF,
     *      JERR)
         IVOL = IX
         SEQNO = IY
         CALL MAPOPN ('READ', IVOL, NAME2, CLASS2, SEQNO, PTYPE, USID,
     *      DLUN, DIND, CNO, CATBLK, BUFF, JERR)
         POTERR = 33
         IF (JERR.GT.1) GO TO 980
         END IF
C                                       find pixel coords
      CALL ADVERB ('PIX2XY', 'R', 7, 0, IDUM, PIXX, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      IF ((PIXX(1).LE.0.5) .OR. (PIXX(1).GE.CATBLK(KINAX)+0.5) .OR.
     *   (PIXX(2).LE.0.5) .OR. (PIXX(2).GE.CATBLK(KINAX+1)+0.5)) THEN
         POTERR = 101
         WRITE (MSGTXT,1100) PIXX
         CALL MSGWRT (8)
         GO TO 970
         END IF
      NAX = CATBLK(KIDIM)
      DO 210 I = 3,7
         IDEPTH(I-2) = 1
         IF (I.LE.NAX) THEN
            IDEPTH(I-2) = PIXX(I) + 0.01
            IDEPTH(I-2) = MAX (1, MIN (CATBLK(KINAX+I-1),
     *         IDEPTH(I-2)))
            END IF
 210     CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH, .FALSE.)
C                                       check axis type
      IF (AXTYP(LOCNUM).EQ.0) THEN
         MSGTXT = 'IMDIST WORKS ONLY ON SKY COORDINATE PAIRS'
         CALL MSGWRT (7)
         POTERR = 101
         GO TO 970
         END IF
      CALL XYVAL (PIXX(1), PIXX(2), SKYPOS(1), SKYPOS(2), SKYPOS(3),
     *   IERR)
      IX = MOD (CORTYP(LOCNUM)-1, 2) + 1
      IF (CORTYP(LOCNUM).GT.3) IX = IX + 1
      IY = 3 - IX
      IY = IY + (CORTYP(LOCNUM)-1) / 2
      RA2 = SKYPOS(IX) * DG2RAD
      DEC2 = SKYPOS(IY) * DG2RAD
      I2TMP1 = IX - 1
      CALL AXSTRN (CTYP(IX,LOCNUM), SKYPOS(IX), I2TMP1, ILEN, AXL(3))
      I2TMP1 = IY - 1
      CALL AXSTRN (CTYP(IY,LOCNUM), SKYPOS(IY), I2TMP1, ILEN, AXL(4))
C                                       display
      MSGTXT = 'The distance between ' // AXL(1) // ' , ' // AXL(2)
      CALL REFRMT (MSGTXT, '_', IX)
      CALL MSGWRT (5)
      MSGTXT = '_________________and ' // AXL(3) // ' , ' // AXL(4)
      CALL REFRMT (MSGTXT, '_', IX)
      CALL MSGWRT (5)
C                                       small angles (< 0.1 asec)
      IF ((ABS(RA1-RA2).LT.5.D-7) .AND. (ABS(DEC1-DEC2).LT.5D-7)) THEN
         DD = SQRT (((RA1-RA2)*COS(DEC1))**2 + (DEC1-DEC2)**2)
      ELSE
         DD = SIN(DEC1)*SIN(DEC2) + COS(DEC1)*COS(DEC2)*COS(RA1-RA2)
         DD = MAX (-1.0D0, MIN (1.0D0, DD))
         DD = ACOS (DD)
         END IF
      ST = 0.0D0
      IF (DD.NE.0.0D0) THEN
         ST = SIN (RA2-RA1) * COS (DEC2) / SIN (DD)
         ST = MAX (-1.0D0, MIN (1.0D0, ST))
         CT = (SIN(DEC2)*COS(DEC1) - COS(DEC2)*SIN(DEC1)*COS(RA2-RA1))
     *       / SIN(DD)
C         ST = RAD2DG * ASIN (ST)
         ST = RAD2DG * ATAN2 (ST, CT)
         END IF
      DD = RAD2DG * 3600.0D0 * DD
      WRITE (MSGTXT,1200) DD, ST
      CALL MSGWRT (5)
      RDUM = DD / 3600.0D0
      CALL ADVRBS ('DIST', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 0
C                                       shifts
      DD = RAD2DG * 3600.0D0 * (RA2 - RA1) * COS (DEC1)
      ST = RAD2DG * 3600.0D0 * (DEC2 - DEC1)
      IF ((ABS(DD).GT.99.0) .OR. (ABS(ST).GT.99.0)) THEN
         WRITE (MSGTXT,1205) DD, ST
      ELSE IF ((ABS(DD).GT.0.99) .OR. (ABS(ST).GT.0.99)) THEN
         WRITE (MSGTXT,1206) DD, ST
      ELSE
         WRITE (MSGTXT,1207) DD, ST
         END IF
      CALL MSGWRT (5)
      GO TO 970
C-----------------------------------------------------------------------
C                                       IMCENTER
C                                       find centroid
C-----------------------------------------------------------------------
 300  IF (DOCIRC) THEN
         WIN(1) = TRC(1) - RADIUS
         WIN(3) = TRC(1) + RADIUS + 0.99
         WIN(2) = TRC(2) - RADIUS
         WIN(4) = TRC(2) + RADIUS + 0.99
      ELSE
         WIN(1) = IROUND (BLC(1))
         WIN(2) = IROUND (BLC(2))
         WIN(3) = IROUND (TRC(1))
         WIN(4) = IROUND (TRC(2))
         END IF
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
      POTERR = 50
      CALL COMOFF (NAX, CATBLK(KINAX), IDEPTH, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970
      OFBLK = OFBLK + 1
      CALL MINIT ('READ', DLUN, DIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   WIN, BUFF, JBUFSZ, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970
      FSUM = 0.0
      XSUM = 0.0
      YSUM = 0.0
      DO 330 IY = 1,NY
         CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, JERR)
         IF (JERR.NE.0) GO TO 970
         IF (DOCIRC) THEN
            DO 310 IX = 1,NX
               R = SQRT ((WIN(1)-1.0+IX-TRC(1))**2 +
     *            (WIN(2)-1.0+IY-TRC(2))**2)
               IF (R.LE.RADIUS) THEN
                  S = RBUFF(IX+ININD-1)
                  IF ((S.NE.FBLANK) .AND. (((FLUX.GE.0.0) .AND.
     *               (S.GE.FLUX)) .OR. ((FLUX.LT.0.0) .AND.
     *               (ABS(S).GE.-FLUX)))) THEN
                     FSUM = FSUM + S
                     XSUM = XSUM + S * IX
                     YSUM = YSUM + S * IY
                     END IF
                  END IF
 310           CONTINUE
         ELSE
            DO 320 IX = 1,NX
               S = RBUFF(IX+ININD-1)
               IF ((S.NE.FBLANK) .AND. (((FLUX.GE.0.0) .AND.
     *            (S.GE.FLUX)) .OR. ((FLUX.LT.0.0) .AND.
     *            (ABS(S).GE.-FLUX)))) THEN
                  FSUM = FSUM + S
                  XSUM = XSUM + S * IX
                  YSUM = YSUM + S * IY
                  END IF
 320           CONTINUE
            END IF
 330     CONTINUE
      IF (FSUM.EQ.0.0) GO TO 960
      PIXX(1) = XSUM / FSUM + WIN(1) - 1.0
      PIXX(2) = YSUM / FSUM + WIN(2) - 1.0
      CALL ADVRBS ('PIXXY', 'R', 7, 0, IDUM, PIXX, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
C                                       no error
      POTERR = 0
      RDUM = -1.0
      CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       do coordinates
      CALL XYVAL (PIXX(1), PIXX(2), SKYPOS(1), SKYPOS(2), SKYPOS(3),
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1132) IERR
         CALL MSGWRT (6)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         END IF
      IF (IPLEV.GT.-1) THEN
         WRITE (MSGTXT,1130) PIXX(1), PIXX(2)
         CALL MSGWRT (5)
         MSGTXT = 'Skypos: '
         ICH = 9
         DO 340 I = 1,2
            I2TMP1 = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I2TMP1, ILEN, CTEMP)
            MSGTXT(ICH:) = CTEMP(:ILEN)
            ICH = ICH + ILEN + 2
 340        CONTINUE
         CALL MSGWRT (5)
C                                       Secondary axes
         IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
            MSGTXT = 'Skypos: '
            ICH = 9
            DO 350 I = 1,2
               IF (NCHLAB(I,LOCNUM).GT.0) THEN
                  MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
                  ICH = ICH + NCHLAB(I,LOCNUM) + 2
                  END IF
 350           CONTINUE
            CALL MSGWRT (5)
            END IF
         POTERR = 0
         END IF
C                                       output COORDINA adverb
      DO 360 I = 1,2
         IF ((CTYP(I,LOCNUM)(:4).EQ.'RA  ') .OR.
     *      (CTYP(I,LOCNUM)(:4).EQ.'RA--')) SKYPOS(I) = SKYPOS(I)/15.0D0
         PFLAG = SKYPOS(I).LT.0.0D0
         SKYPOS(I) = ABS (SKYPOS(I))
         IX = SKYPOS(I)
         XCOORD(3*I-2) = IX
         SKYPOS(I) = (SKYPOS(I) - IX) * 60.0D0
         IX = SKYPOS(I)
         XCOORD(3*I-1) = IX
         XCOORD(3*I) = (SKYPOS(I) - IX) * 60.0D0
         IF (PFLAG) THEN
            XCOORD(3*I-2) = -XCOORD(3*I-2)
            XCOORD(3*I-1) = -XCOORD(3*I-1)
            XCOORD(3*I) = -XCOORD(3*I)
            END IF
 360     CONTINUE
      CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
      GO TO 970
C-----------------------------------------------------------------------
C                                       Fit fails.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      POTERR = 0
      RDUM = 1.0
      CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       close up map
 970  CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F, BUFF, JERR)
C                                       standard POPS error handling
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PIXXY=',2F7.1,5F6.0,' OUT OF RANGE')
 1130 FORMAT ('Map pixel position: ',2F8.2)
 1132 FORMAT ('ERROR',I3,' FINDING SKY COORDINATES')
 1145 FORMAT (A,' = ',E11.4,1X,A8)
 1146 FORMAT (A,' = ',F8.3,1X,A5,1X,A8)
 1147 FORMAT ('RASHIFT=',F11.6,' DECSHIFT=',F11.6,' to center on pixel')
 1200 FORMAT ('is',F11.6,' arc seconds at position angle',F8.2,
     *   ' degrees')
 1205 FORMAT ('Shift =',2F11.2,' arc seconds')
 1206 FORMAT ('Shift =',2F11.5,' arc seconds')
 1207 FORMAT ('Shift =',2F11.7,' arc seconds')
 1960 FORMAT ('FIT FAILS')
      END
