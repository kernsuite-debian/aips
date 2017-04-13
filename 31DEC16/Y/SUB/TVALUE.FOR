      SUBROUTINE TVALUE (LUN, GR, TVCH, BUFFER, RBUFF, POTERR, IERR)
C-----------------------------------------------------------------------
C! verb to display image value at pixel indicated by TV cursor (CURVAL)
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2006, 2008-2009, 2014-2016
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
C   TVALUE performs interactive displays of map image values:
C   Inputs:
C      LUN      I      LUN to use for disk I/O
C      GR       I      Graphics plane for lettering
C      TVCH     I      > 0 => only use images in this TV channel
C   Output:
C      BUFFER   I(*)   Scratch buffer
C      RBUFF    R(*)   Real buffer - can be equiv to BUFFER
C      POTERR   I      Error code for AIPS itself
C      IERR     I      Basic TV error code
C-----------------------------------------------------------------------
      INTEGER   LUN, GR, TVCH, BUFFER(*), POTERR, IERR
      REAL      RBUFF(*)
C
      CHARACTER STRING*16, PREFIX*5, PHNAME*48, ITRTYP(8)*2,
     *   LMTYPS(2)*2, LMTYPE*2
      INTEGER   MIND, IG, IG1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0,
     *   IX, IY, IP, ECOUNT, QUAD, IBUT, ITR, BLKOF, ICMASK, IWIN(4),
     *   IPOS, ZAND, LQUAD, ISCX, ISCY, I, INCNO, INVOL, LDEP(5), ITG1,
     *   ITG2, ITEMP, IX1, IY1, TSTCAT(256), MSGSAV, SCRTCH(256), CH1,
     *   CH2, NCH
      REAL   PPOS(2), RPOS(2), DS, DO, PIXVAL, CORN(7), DZ, DLO
      LOGICAL   T, F, EQUAL, DOIT, OVER(4), FROMTV, BLNKD, LFRMTV,
     *   NOQUAD, WASNOP, BADCAT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ITRTYP /'LN','LG','L2','SQ','NE','NG','N2','NQ'/
      DATA LMTYPS /'WE','ZZ'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF ((TVCH.LE.0) .OR. (TVCH.GT.NGRAY)) THEN
         CH1 = 1
         CH2 = NGRAY
      ELSE
         CH1 = TVCH
         CH2 = TVCH
         END IF
      WASNOP = .FALSE.
      MIND = 0
      LQUAD = 0
      QUAD = 0
      POTERR = 101
      FROMTV = .FALSE.
      CALL ZTIME (ITW)
      MSGSAV = MSGSUP
      BADCAT = .FALSE.
C                                       Overlapped image mode or
C                                       faster one?
      CALL YCOVER (OVER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 960
      NOQUAD = (TVLIMG(1).EQ.TVLIMG(2)) .AND. (TVLIMG(1).EQ.TVLIMG(3))
     *   .AND. (TVLIMG(1).EQ.TVLIMG(4))
C                                       Turn on graphics
      POTERR = 0
      IG1 = MIN (GR, NGRAPH)
      IF (IG1.LE.0) IG1 = MIN (2, NGRAPH)
      IG2 = 0
      IF (NGRAPH.GE.4) IG2 = NGRAPH
      ITG1 = NGRAY + IG1
      ITG2 = NGRAY + IG2
      CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (IG2.NE.0) THEN
         CALL YSLECT ('ONNN', ITG2, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Display area: location, size
C                                       Approx corr for zoom
      NCH = 13
      NPIX = NCH * CSIZTV(1)
      NROW = 4 * CSIZTV(2)
      ISCX = 0
      ISCY = 0
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2) - TVSCGX
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3) - TVSCGY
C     IX = MOD (IX0 - 1 - TVSCGX + 3*MAXXTV(1), MAXXTV(1)) + 1
C     IY = MOD (IY0 - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
C                                       Turn off scroll if needed
C     IF ((IX+NPIX-1.GT.WINDTV(3)) .OR. (IY+NROW-1.GT.WINDTV(4))) THEN
C        ISCX = TVSCGX
C        ISCY = TVSCGY
C        I = 0
C        ITEMP = 2 ** NGRAY
C        CALL YSCROL (ITEMP, I, I, F, IERR)
C        IF (IERR.NE.0) GO TO 970
C        IX = IX0
C        IY = IY0
C        END IF
C                                       Fill with 1's, 0's
C     IX0 = MAX (1, IX)
C     IY0 = MAX (1, IY)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      CALL YHOLD ('ONNN', IERR)
      CALL YFILL (ITG1, IX0, IY0, IX1, IY1, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (IG2.GT.0) THEN
         CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                       CURVALUE (from disk file)
C                                       no image yet
      CALL FILL (4, 0, CATBLK(IICOR))
      CATBLK(IICNO) = 0
      LDEP(1) = -10000
      WRITE (MSGTXT,1100)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1101)
      CALL MSGWRT (1)
C                                       turn on cursor
      IP = 0
      ECOUNT = 0
      IG = IG1 + NGRAY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Cursor read loop point
 110  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      IF ((IBUT.GE.8) .OR. (IERR.NE.0)) GO TO 970
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (DOIT) THEN
C                                       Find new image catalog block
         QUAD = 0
         CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IERR)
         IX = RPOS(1) + 0.51
         IY = RPOS(2) + 0.51
         INCNO = CATBLK(IICNO)
         INVOL = CATBLK(IIVOL)
         IF (NOQUAD) LQUAD = QUAD
         IF ((IX.GE.CATBLK(IICOR)) .AND. (IX.LE.CATBLK(IICOR+2)) .AND.
     *      (IY.GE.CATBLK(IICOR+1)) .AND. (IY.LE.CATBLK(IICOR+3)) .AND.
     *      (LQUAD.EQ.QUAD) .AND. (.NOT.OVER(QUAD))) GO TO 150
            LQUAD = QUAD
            DO 115 IP = CH1,CH2
               ITEMP = 2 ** (IP - 1)
               IF (ZAND(TVLIMG(QUAD),ITEMP).NE.0) THEN
                  CALL YCREAD (IP, IX, IY, CATBLK, IERR)
                  IF (IERR.EQ.0) GO TO 120
                  IF (IERR.NE.1) GO TO 960
                  END IF
 115           CONTINUE
C                                       No or invalid image here
 116        ECOUNT = ECOUNT + 1
            CALL FILL (4, 0, CATBLK(IICOR))
            CATBLK(IICNO) = 0
            LQUAD = 0
            IF (ECOUNT.EQ.1) THEN
               WRITE (MSGTXT,1116) IX, IY
               CALL MSGWRT (1)
               END IF
            GO TO 110
C                                       Set up image reads
C                                       Open map files
 120        CALL H2CHR (2, KHPTYO, CATH(KHPTY), LMTYPE)
            IF (LMTYPE.EQ.LMTYPS(2)) GO TO 116
            BLNKD = .FALSE.
            LFRMTV = FROMTV
            FROMTV = (LMTYPE.EQ.LMTYPS(1)) .OR. (CATBLK(IIVOL).LE.0)
     *         .OR. (CATBLK(IICNO).LE.0) .OR.
     *         ((NLUSER.NE.CATBLK(KIIMU)) .AND. (UCTSIZ.GT.0))
            IF (FROMTV) GO TO 130
               CALL COMPAR (5, CATBLK(IIDEP), LDEP, EQUAL)
               IF ((INCNO.EQ.CATBLK(IICNO)) .AND. (INVOL.EQ.
     *            CATBLK(IIVOL)) .AND. (MIND.GT.0) .AND. (EQUAL)
     *            .AND. (.NOT.LFRMTV)) GO TO 150
               IF ((INCNO.EQ.CATBLK(IICNO)) .AND. (INVOL.EQ.
     *            CATBLK(IIVOL)) .AND. (MIND.EQ.0) .AND. (EQUAL)
     *            .AND. (LFRMTV)) THEN
                   FROMTV = .TRUE.
                   GO TO 150
                   END IF
               IF (MIND.GT.0) CALL ZCLOSE (LUN, MIND, IERR)
               MIND = 0
               IF (LMTYPE.NE.'MA') GO TO 116
               CALL ZPHFIL ('MA', CATBLK(IIVOL), CATBLK(IICNO), 1,
     *            PHNAME, IERR)
C                                       test catalog header more
               MSGSUP = 32000
               CALL CATIO ('READ', CATBLK(IIVOL), CATBLK(IICNO), TSTCAT,
     *            'REST', SCRTCH, IERR)
               MSGSUP = MSGSAV
               IF (IERR.GT.5) IERR = 0
               IF (IERR.EQ.0) THEN
                  DO 125 I = 1,112
                     IF (TSTCAT(I).NE.CATBLK(I)) IERR = 2
 125                 CONTINUE
                  END IF
               MSGSUP = 32000
               IF (IERR.EQ.0) CALL ZOPEN (LUN, MIND, CATBLK(IIVOL),
     *            PHNAME, T, F, T, IERR)
               MSGSUP = MSGSAV
               WASNOP = IERR.NE.0
               IF (WASNOP) THEN
                  MIND = 0
                  BADCAT = .TRUE.
                  GO TO 130
                  END IF
               ITR = 1
               DS = 1.0
               DO = 0.0
               CALL COPY (5, CATBLK(IIDEP), LDEP)
               CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), LDEP, BLKOF,
     *            IERR)
               IF (IERR.NE.0) GO TO 116
               BLKOF = BLKOF + 1
               ECOUNT = 0
               GO TO 150
C                                       Scaling parms for TV pixvals
 130        CONTINUE
               CALL COPY (5, CATBLK(IIDEP), LDEP)
               FROMTV = .TRUE.
               ICMASK = 2 ** (IP-1)
               ITR = 1
               CALL H2CHR (2, 1, CATH(IITRA), LMTYPE)
               DO 135 I = 1,8
                  IF (LMTYPE.EQ.ITRTYP(I)) ITR = I
 135              CONTINUE
               ECOUNT = 0
               IF (CATH(IRRAN).EQ.CATR(IRRAN+1)) THEN
                  CATH(IRRAN) = CATR(IRRAN) - 0.5
                  CATH(IRRAN+1) = CATR(IRRAN+1) + 0.5
                  END IF
               DS = (CATH(IRRAN+1) - CATR(IRRAN)) / (MAXINT - 0.02)
               IF (ITR.EQ.5) THEN
                  DS = -DS
                  DO = CATH(IRRAN) - (MAXINT + 0.49) * DS
               ELSE IF (ITR.EQ.1) THEN
                  DO = CATH(IRRAN) - 0.51 * DS
               ELSE IF (ITR.EQ.4) THEN
                  DS = DS / (MAXINT - 0.02)
                  DO = CATH(IRRAN)
               ELSE IF (ITR.EQ.8) THEN
                  DS = -DS / (MAXINT - 0.02)
                  DO = CATH(IRRAN+1)
               ELSE IF ((ITR.EQ.2) .OR. (ITR.EQ.6)) THEN
                  DLO = 9.0 / (CATH(IRRAN+1) - CATR(IRRAN))
                  DS = LOG (DLO * (CATH(IRRAN+1)-CATR(IRRAN))+1.0) /
     *               (MAXINT - 0.02)
                  DZ = -0.51 * DS
                  DO = DLO * CATH(IRRAN) - 1.0
                  IF (ITR.EQ.6) DO = DLO * CATH(IRRAN+1) + 1.0
               ELSE IF ((ITR.EQ.3) .OR. (ITR.EQ.7)) THEN
                  DLO = 99.0 / (CATH(IRRAN+1) - CATR(IRRAN))
                  DS = LOG (DLO * (CATH(IRRAN+1)-CATR(IRRAN))+1.0) /
     *               (MAXINT - 0.02)
                  DZ = -0.51 * DS
                  DO = DLO * CATH(IRRAN) - 1.0
                  IF (ITR.EQ.6) DO = DLO * CATH(IRRAN+1) + 1.0
                  END IF
C                                       Get pixel value
 150     IF (.NOT.FROMTV) THEN
            CALL IMA2MP (RPOS, CORN)
            IX = CORN(1) + 0.51
            IY = CORN(2) + 0.51
            IWIN(1) = IX
            IWIN(3) = IX
            IWIN(2) = IY
            IWIN(4) = IY
            CALL MINIT ('READ', LUN, MIND, CATBLK(KINAX),
     *         CATBLK(KINAX+1), IWIN, RBUFF, 512, BLKOF, IERR)
            IF (IERR.NE.0) GO TO 950
            CALL MDISK ('READ', LUN, MIND, RBUFF, IPOS, IERR)
            IF (IERR.NE.0) GO TO 950
            PIXVAL = RBUFF(IPOS)
            BLNKD = PIXVAL.EQ.FBLANK
C                                       From TV for wedges
         ELSE
            CALL YIMGIO ('READ', IP, IX, IY, 0, 1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            PIXVAL = BUFFER(1)
            BLNKD = BUFFER(1).EQ.0
            CALL IMA2MP (RPOS, CORN)
            IX = CORN(1) + 0.51
            IY = CORN(2) + 0.51
C                                       Scale intensity
            IF (.NOT.BLNKD) THEN
               IF ((ITR.EQ.2) .OR. (ITR.EQ.3)) THEN
                  PIXVAL = (EXP(PIXVAL * DS + DZ) + DO) / DLO
               ELSE IF ((ITR.EQ.6) .OR. (ITR.EQ.7)) THEN
                  PIXVAL = (DO - EXP(PIXVAL * DS + DZ)) / DLO
               ELSE IF ((ITR.EQ.4) .OR. (ITR.EQ.8)) THEN
                  PIXVAL = (PIXVAL - 0.51)**2 * DS + DO
               ELSE
                  PIXVAL = PIXVAL * DS + DO
                  END IF
               END IF
            END IF
C                                       Write text to TV
         IF ((IX.LE.9999) .AND. (IY.LE.9999)) THEN
            WRITE (STRING,1170) IX, IY
         ELSE
            WRITE (STRING,1171) IX, IY
            END IF
         IY = IY0 + 3*CSIZTV(2)
         CALL YHOLD ('ONNN', IERR)
         CALL YSLECT ('OFFF', ITG1, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:NCH), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.BLNKD) THEN
            CALL METSCA (PIXVAL, PREFIX, EQUAL)
            WRITE (STRING,1172) PIXVAL
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            STRING = PREFIX
            CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:NCH), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
         ELSE
            STRING = 'B  BLANKED'
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            STRING = ' '
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:NCH), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         IF (IG2.GT.0) THEN
            CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL YHOLD ('OFFF', IERR)
         END IF
      GO TO 110
C-----------------------------------------------------------------------
C                                       Close downs
C                                       Disk IO error
 950  WRITE (MSGTXT,1950) IERR
      CALL MSGWRT (6)
      POTERR = 50
      GO TO 975
C                                       Img Catlg error
 960  WRITE (MSGTXT,1960) IERR
      CALL MSGWRT (6)
      POTERR = 33
      GO TO 975
C                                       TV error possibly
 970  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1970) IERR
         CALL MSGWRT (6)
         POTERR = 49
      ELSE IF (BADCAT) THEN
         MSGTXT = 'WARNING: disk headers did not always match image'
     *      // ' catalog headers'
         CALL MSGWRT (6)
         END IF
C                                       Close things
 975  IF (MIND.GT.0) CALL ZCLOSE (LUN, MIND, IERR)
      CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IERR)
      ITEMP = 2 ** NGRAY
      IF ((ISCX.NE.0) .OR. (ISCY.NE.0)) CALL YSCROL (ITEMP, ISCX,
     *   ISCY, F, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Cursor selects which pixel is displayed')
 1101 FORMAT ('Hit button D to exit')
 1116 FORMAT ('TVALUE: ',2I7,' NOT IN VALID IMAGE')
 1170 FORMAT ('X=',I4,' Y=',I4)
 1171 FORMAT (I6,I7)
 1172 FORMAT ('B=',F8.3)
 1950 FORMAT ('TVALUE: MAP FILE IO ERROR',I7)
 1960 FORMAT ('TVALUE: IMAGE CAT FILE IO ERROR',I7)
 1970 FORMAT ('TVALUE: TV ACTION IO ERROR',I7)
      END
