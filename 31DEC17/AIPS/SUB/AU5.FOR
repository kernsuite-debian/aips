       SUBROUTINE AU5 (BRANCH)
C-----------------------------------------------------------------------
C! basic TV verbs to do on/off, read cursor position, init the TV, ...
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2003, 2006, 2008-2009, 2012
C;  Copyright (C) 2014-2015
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
C   Do most common image functions:
C   BRANCH =   1 => initialize image device
C              2 => clear image channel(s)
C              3 => clear graphics channel(s)
C              4 => turn on selected image channel(s)
C              5 => turn off image channel(s)
C              6 => turn on graphics channel(s)
C              7 => turn off graphics
C              8 => select standard color channels
C              9 => read cursor position in screen coord
C             10 => read cursor position in map coord
C             11 => read cursor position in sky coord
C             12 => use cursor to fill in INNAME, INCLASS, INSEQ
c                   from selected image
C             13 => Switch cursor to/from blinking
C             14 => Read graphics channel color
C             15 => Write graphics channel color
C             16 => Write character multiply factor
C             17 => Set JMFIT/IMFIT initial guess adverbs
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER RSTR*20, ONCODE(2)*4, NAME*18, CLASS*6, TYPE*2,
     *   PRGNAM*6, CDUM*1, VNAME(3)*6
      INTEGER   IERR, IPL, ICHAN, INCH, INBUF(MABFSS), ICOLOR, JON,
     *   JTVPOS, DUM, IN1, IN2, I, ICH, ILEN, IX, IY, JERR, POTERR, IGR,
     *   ITEMP, ZAND, ZOR, IDUM, IC, MSGSAV, NBO, MBOX, NG, QUAD, DLUN,
     *   DIND, JBUFSZ, WIN(4), OFBLK, IBUT, CNO, SEQNO, IVOL, NX, NY,
     *   CATIMG(256), ININD, IXC(33), IYC(33)
      LOGICAL   F, LGRAPH, PFLAG, ROAMED
      REAL      EPS, RPOS(2), RDUM, XYPOS(7), XCOORD(6), BLCO(7),
     *   TRCO(7), GMAX(4), GPOS(2,4), GWID(3,4), ARRAY(16,16), DX(2),
     *   RBUFF(MABFSS), SLOPE, OFFS, CG, SG, X, Y
      DOUBLE PRECISION SKYPOS(3)
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (INBUF, RBUFF)
      COMMON /AIPSCR/ INBUF
      DATA PRGNAM /'AU5 '/
      DATA F /.FALSE./
      DATA JON, JTVPOS /4, 9/
      DATA ONCODE /'ONNN', 'OFFF'/
      DATA EPS /.01/
      DATA VNAME /'TVPOS', 'IMXY', 'IMPOS'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.17)) GO TO 999
      POTERR = 49
      CALL TVOPEN (CATBLK, JERR)
      IF (JERR.NE.0) THEN
         POTERR = 101
         GO TO 990
         END IF
      ROAMED = (TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3))
     *   .OR. (TVLIMG(1).NE.TVLIMG(4))
      MSGSAV = MSGSUP
      JBUFSZ = 2 * MABFSS
C
      GO TO (100, 200, 300, 400, 400, 400, 400, 500, 600, 600, 600,
     *   650, 700, 750, 750, 800, 650), BRANCH
C-----------------------------------------------------------------------
C                                       TVINIT
C                                       initialize subunits
C-----------------------------------------------------------------------
 100  CALL YINIT (INBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1100) JERR
         GO TO 980
         END IF
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVCLEAR
C                                       clear gray img & catalog
C-----------------------------------------------------------------------
 200  CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IF ((ICHAN.GE.1) .AND. (ICHAN.LE.NGRAY)) THEN
         IC = 2 ** (ICHAN-1)
      ELSE
         ITEMP = 2 ** NGRAY - 1
         IC = ZAND (IC, TVLIMG(1))
         IC = ZOR (IC, TVLIMG(2))
         IC = ZOR (IC, TVLIMG(3))
         IC = ZOR (IC, TVLIMG(4))
         END IF
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
      CALL YHOLD ('ONNN', JERR)
      DO 210 IP = 1,NGRAY
         ITEMP = 2**(IP-1)
         IF (ZAND(IC,ITEMP).NE.0) THEN
            CALL YCINIT (IP, INBUF)
            CALL YZERO (IP, JERR)
            IF (JERR.NE.0) GO TO 985
            END IF
 210     CONTINUE
      CALL YHOLD ('OFFF', JERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       GRCLEAR
C                                       clear graphics plane
C-----------------------------------------------------------------------
 300  CALL ADVERB ('GRCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IF ((ICHAN.GT.0) .AND. (ICHAN.LE.NGRAPH)) THEN
         IC = 2 ** (ICHAN-1)
      ELSE
         IC = 2 ** NGRAPH - 1
         END IF
      IN1 = NGRAY + 1
      IN2 = NGRAY + NGRAPH
      CALL YHOLD ('ONNN', JERR)
      DO 310 IP = IN1,IN2
         ITEMP = 2 ** (IP-IN1)
         IF (ZAND(IC, ITEMP).NE.0) THEN
            CALL YCINIT (IP, INBUF)
            CALL YZERO (IP, JERR)
            IF (JERR.NE.0) GO TO 985
            END IF
 310     CONTINUE
      CALL YHOLD ('OFFF', JERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVON
C                                       TVOFF
C                                       GRON
C                                       GROFF
C                                       select TV channels on/off
C-----------------------------------------------------------------------
 400  IF ((ROAMED) .AND. (BRANCH.EQ.4)) THEN
         POTERR = 77
         GO TO 985
         END IF
      DUM = BRANCH - JON
      DUM = MOD (DUM , 2)
      LGRAPH = ((BRANCH - JON)/2.EQ.1)
      INCH = NGRAY
      IF (LGRAPH) INCH = NGRAPH
      IC = 0
      IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
         IC = V(SP) + EPS
         SP = SP - 1
         END IF
      IF (IC.LE.0) THEN
         IF (LGRAPH) THEN
            CALL ADVERB ('GRCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
         ELSE
            CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
            END IF
         IF (ERRNUM.NE.0) GO TO 985
         IF ((ICHAN.GT.0) .AND. (ICHAN.LE.INCH)) IC = 2 ** (ICHAN-1)
         END IF
      IF ((BRANCH.EQ.7) .AND. (IC.EQ.0)) IC = 2 ** NGRAPH - 1
      CALL ADVERB ('COLORS', 'I', 1, 0, ICOLOR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IF ((ICOLOR.LT.0) .OR. (ICOLOR.GT.3)) THEN
         POTERR = 101
         WRITE (MSGTXT,1410) ICOLOR
         GO TO 980
         END IF
      IF (IC.EQ.0) IC = 1
      IF (LGRAPH) THEN
         IN1 = 1 + NGRAY
         IN2 = NGRAPH + NGRAY
         IC = IC * (2**NGRAY)
      ELSE
         IN1 = 1
         IN2 = NGRAY
         END IF
      CALL YHOLD ('ONNN', JERR)
      DO 425 IP = IN1,IN2
         ITEMP = 2**(IP-1)
         IF (ZAND (IC,ITEMP).NE.0) THEN
            CALL YSLECT (ONCODE(1+DUM), IP, ICOLOR ,INBUF, JERR)
            IF (JERR.NE.0) GO TO 985
            END IF
 425     CONTINUE
      CALL YHOLD ('OFFF', JERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       TV3COLOR
C                                       set standard RGB in chan 123
C-----------------------------------------------------------------------
C                                       turn all off first
 500  IF (ROAMED) THEN
         POTERR = 77
         GO TO 985
         END IF
      CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      ICHAN = MAX (1, MIN (NGRAY-2, ICHAN))
      CALL YHOLD ('ONNN', JERR)
      DO 510  IP = 1,NGRAY
         CALL YSLECT ('OFFF', IP, 0, INBUF, JERR)
         IF (JERR.NE.0) GO TO 985
 510     CONTINUE
      DO 520  IP = 1,3
         CALL YSLECT ('ONNN', ICHAN, IP, INBUF, JERR)
         IF (JERR.NE.0) GO TO 985
         ICHAN = ICHAN + 1
 520     CONTINUE
      CALL YHOLD ('OFFF', JERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVPOS
C                                       IMXY
C                                       IMPOS
C                                       read cursor to get position
C-----------------------------------------------------------------------
 600  MSGTXT = VNAME(BRANCH-8)(:5) // ': position TV cursor, then push'
     *   // ' button A, B, C, or D'
      CALL MSGWRT (1)
      CALL TVWHER (QUAD, RPOS, IBUT, JERR)
      IF (JERR.NE.0) GO TO 985
      CALL ADVRBS ('TVBUT', 'I', 1, 0, IBUT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IF (BRANCH.EQ.JTVPOS) THEN
         CALL ADVRBS ('TVXY', 'R', 2, 0, IDUM, RPOS, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
         GO TO 970
         END IF
C                                       image pix -> map pixel pos
      IX = RPOS(1) + EPS
      IY = RPOS(2) + EPS
C                                       Find lowest plane with x,y
      IN2 = NGRAY + NGRAPH
      DO 610 IP = 1,IN2
C                                       skip off channels
         ITEMP = 2**(IP-1)
         IF (ZAND (TVLIMG(QUAD), ITEMP).NE.0) THEN
C                                       get img cat block
            CALL YCREAD (IP, IX, IY, CATBLK, IERR)
            IF (IERR.EQ.0) GO TO 620
C                                       loop if x,y not in image
            IF (IERR.NE.1) GO TO 985
            END IF
 610     CONTINUE
C                                       x,y not in on image
      WRITE (MSGTXT,1610) IX, IY
      CALL MSGWRT (6)
      GO TO 970
C                                       image -> map positions
 620  CALL IMA2MP (RPOS, RPOS)
      WRITE (MSGTXT,1620) RPOS
      CALL MSGWRT (5)
      IF (BRANCH.EQ.JTVPOS+1) THEN
         XYPOS(1) = RPOS(1)
         XYPOS(2) = RPOS(2)
         DO 625 I = 3,7
            XYPOS(I) = CATBLK(IIDEP+I-3)
 625        CONTINUE
         CALL ADVRBS ('PIXXY', 'R', 7, 0, IDUM, XYPOS, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
C                                       map -> sky positions
      ELSE
         LOCNUM = 1
         CALL MP2SKY (RPOS, SKYPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1625) IERR
            CALL MSGWRT (6)
            GO TO 970
            END IF
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
C                                       Primary axes
         MSGTXT = 'IMPOS: '
         ICH = 8
         DO 630 I = 1,2
            ITEMP = I-1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), ITEMP, ILEN, RSTR)
            MSGTXT(ICH:) = RSTR(:ILEN)
            ICH = ICH + ILEN + 2
            IF ((CTYP(I,LOCNUM)(:4).EQ.'RA  ') .OR.
     *         (CTYP(I,LOCNUM)(:4).EQ.'RA--')) SKYPOS(I) = SKYPOS(I) /
     *         15.0D0
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
 630        CONTINUE
         CALL MSGWRT (5)
         CALL ADVRBS ('COORDINA', 'R', 6, 0, IDUM, XCOORD, CDUM)
C                                       Secondary axes values
         IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
            ICH = 8
            MSGTXT(ICH:) = ' '
            DO 635 I = 1,2
               IF (NCHLAB(I,LOCNUM).GT.0) THEN
                  MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
                  ICH = ICH + NCHLAB(I,LOCNUM) + 2
                  END IF
 635           CONTINUE
            CALL MSGWRT (5)
            END IF
         END IF
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVNAME
C                                       read cursor to set name adverbs
C-----------------------------------------------------------------------
 650  IX = NGRAY + NGRAPH
      MSGSUP = 32000
      CALL TVFIND (IX, 'MA', IP, LGRAPH, CATBLK, INBUF, JERR)
      MSGSUP = MSGSAV
C                                       roamed - find anyway
      IF (JERR.EQ.4) THEN
         MSGTXT = 'TVNAME: position TV cursor, then push'
     *      // ' button A, B, C, or D'
         CALL MSGWRT (1)
         CALL TVWHER (QUAD, RPOS, IBUT, JERR)
         IF (JERR.NE.0) GO TO 985
         IX = RPOS(1) + EPS
         IY = RPOS(2) + EPS
         IN2 = NGRAY + NGRAPH
         DO 660 IP = 1,IN2
C                                       skip off channels
            ITEMP = 2**(IP-1)
            IF (ZAND (TVLIMG(QUAD), ITEMP).NE.0) THEN
               CALL YCREAD (IP, IX, IY, CATBLK, JERR)
C                                       loop if x,y not in image
               IF (JERR.NE.1) GO TO 670
               END IF
 660        CONTINUE
         END IF
 670  IF (JERR.NE.0) GO TO 985
C                                       set name adverbs
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), TYPE)
      CALL ADVRBS ('INNAME', 'C', 1, 12, IDUM, RDUM, NAME(:12))
      IF (ERRNUM.NE.0) GO TO 985
      CALL ADVRBS ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASS)
      IF (ERRNUM.NE.0) GO TO 985
      CALL ADVRBS ('INTYPE', 'C', 1, 2, IDUM, RDUM, TYPE)
      IF (ERRNUM.NE.0) GO TO 985
      SEQNO = CATBLK(KIIMS)
      CALL ADVRBS ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IVOL = CATBLK(IIVOL)
      IF ((IVOL.GT.0) .AND. (IVOL.LE.NVOL)) THEN
         CALL ADVRBS ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
         END IF
      CALL ADVERB ('INDISK', 'I', 1, 0, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      WRITE (MSGTXT,1670) CATBLK(KIIMU), I, TYPE
      NAME(13:) = CLASS
      CALL NAMEST (NAME, CATBLK(KIIMS), MSGTXT(40:), I)
      CALL MSGWRT (2)
      IF (BRANCH.EQ.17) GO TO 850
      GO TO 970
C-----------------------------------------------------------------------
C                                       CURBLINK
C                                       Switch cursor blink / steady
C-----------------------------------------------------------------------
 700  CALL YCURSE ('BLNK', F, F, RPOS, IPL, ICH, JERR)
      IF (JERR.NE.0) GO TO 985
      GO TO 970
C-----------------------------------------------------------------------
C                                       GREAD/GWRITE
C                                       read/write graphics colrs
C-----------------------------------------------------------------------
 750  CALL ADVERB ('GRCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      IF ((ICHAN.LT.0) .OR. (ICHAN.GT.NGRAPH+1)) THEN
         WRITE (MSGTXT,1750) ICHAN, NGRAPH
         POTERR = 101
         GO TO 980
      ELSE IF (ICHAN.EQ.NGRAPH+1) THEN
         ICHAN = 0
         END IF
C                                       Do the read
      IF (BRANCH.EQ.14) THEN
         CALL YGRAFX ('READ', ICHAN, XYPOS(1), XYPOS(2), XYPOS(3),
     *      JERR)
         IF (JERR.NE.0) GO TO 985
         IF (ICHAN.NE.0) THEN
            WRITE (MSGTXT,1760) ICHAN, XYPOS(1), XYPOS(2), XYPOS(3)
         ELSE
            WRITE (MSGTXT,1761) XYPOS(1), XYPOS(2), XYPOS(3)
            END IF
         CALL MSGWRT (3)
         CALL ADVRBS ('RGBCOLOR', 'R', 3, 0, IDUM, XYPOS, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
C                                       Check Write values
      ELSE
         CALL ADVERB ('RGBCOLOR', 'R', 3, 0, IDUM, XYPOS, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
         IF ((XYPOS(1).LT.0.0) .OR. (XYPOS(1).GT.1.0) .OR.
     *      (XYPOS(2).LT.0.0) .OR. (XYPOS(2).GT.1.0) .OR.
     *      (XYPOS(3).LT.0.0) .OR. (XYPOS(3).GT.1.0)) THEN
            POTERR = 101
            WRITE (MSGTXT,1770) XYPOS(1), XYPOS(2), XYPOS(3)
            GO TO 980
            END IF
         IF ((XYPOS(1).LT.0.1) .AND. (XYPOS(2).LT.0.1) .AND.
     *      (XYPOS(3).LT.0.1)) THEN
            IF (ICHAN.EQ.0) THEN
               MSGTXT = 'COLORS TOO DARK FOR A CURSOR'
               POTERR = 101
               GO TO 980
            ELSE IF (ICHAN.NE.NGRAPH) THEN
               MSGTXT = 'You may not like this dark color'
               CALL MSGWRT (6)
               END IF
         ELSE IF (ICHAN.EQ.NGRAPH) THEN
            MSGTXT = 'You may not like this bright background color'
            CALL MSGWRT (6)
            END IF
         CALL YGRAFX ('WRIT', ICHAN, XYPOS(1), XYPOS(2), XYPOS(3),
     *      JERR)
         IF (JERR.NE.0) GO TO 985
         END IF
      GO TO 970
C-----------------------------------------------------------------------
C                                       CHARMULT
C                                       multiply tv character
C-----------------------------------------------------------------------
 800  IC = 0
      IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
         IC = V(SP) + EPS
         SP = SP - 1
         END IF
      IF ((IC.LE.0) .OR. (IC.GT.5)) THEN
         CALL ADVERB ('FACTOR', 'I', 1, 0, IC, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 985
         IF ((IC.LE.0) .OR. (IC.GT.5)) THEN
            MSGTXT = 'FACTOR OUT OF RANGE 1-5'
            POTERR = 27
            GO TO 980
            END IF
         END IF
      POTERR = 49
      CALL YCMULT (IC, JERR)
      IF (JERR.NE.0) GO TO 985
      GO TO 970
C-----------------------------------------------------------------------
C                                       MFITSET
C                                       set JMFIT initial guess
C-----------------------------------------------------------------------
C                                       TVWIN set BLC, TRC
 850  POTERR = 31
      IGR = MIN (3, NGRAPH)
      IF (IGR.LE.0) GO TO 985
      IPL = IGR + NGRAY
      CALL YCINIT (IPL, INBUF)
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, TRCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      NBO = 0
      MBOX = 1
      CALL GRBOXS (IGR, MBOX, NBO, BLCO, TRCO, INBUF, IERR)
      POTERR = 49
      IF (IERR.NE.0) GO TO 985
C                                       store BLC, TRC
      POTERR = 0
      WRITE (MSGTXT,1850) 'BLC', BLCO
      CALL MSGWRT (2)
      WRITE (MSGTXT,1850) 'TRC', TRCO
      CALL MSGWRT (2)
      CALL ADVRBS ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      CALL ADVRBS ('TRC', 'R', 7, 0, IDUM, TRCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
      CALL ADVERB ('GRCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 985
C                                       open map file
      DLUN = 16
      CALL MAPOPN ('READ', IVOL, NAME, CLASS, SEQNO, TYPE, NLUSER, DLUN,
     *    DIND, CNO, CATIMG, INBUF, JERR)
      POTERR = 33
      IF (JERR.GT.1) GO TO 985
      NG = 0
      CALL RFILL (4, 0.0, GMAX)
      CALL RFILL (8, 0.0, GPOS)
      CALL RFILL (12, 0.0, GWID)
      POTERR = 49
C                                       Gaussian loop
 860  IF (NG.LT.4) THEN
         NG = NG + 1
         WRITE (CDUM,1860) NG
         MSGTXT = '*** Position cursor at peak of component ' // CDUM
         CALL MSGWRT (1)
         MSGTXT = 'Then hit button A, B, or C; hit D to quit'
         CALL MSGWRT (1)
         CALL TVWHER (QUAD, RPOS, IBUT, JERR)
         IF (JERR.NE.0) GO TO 960
         IF (IBUT.LT.8) THEN
            CALL IMA2MP (RPOS, RPOS)
            IF ((RPOS(1).LT.BLCO(1)) .OR. (RPOS(1).GT.TRCO(1)) .OR.
     *         (RPOS(2).LT.BLCO(2)) .OR. (RPOS(2).GT.TRCO(2))) THEN
               MSGTXT = 'POSITION OUTSIDE WINDOW'
               CALL MSGWRT (8)
               GO TO 960
               END IF
            NX = 5
            NY = 5
            WIN(1) = RPOS(1) - 0.5 * NX + 1.
            WIN(1) = MAX (1, WIN(1))
            WIN(2) = RPOS(2) - 0.5 * NY + 1.
            WIN(2) = MAX (1, WIN(2))
            WIN(3) = WIN(1) + NX - 1
            WIN(3) = MIN (WIN(3), CATIMG(KINAX))
            WIN(4) = WIN(2) + NY - 1
            WIN(4) = MIN (WIN(4), CATIMG(KINAX+1))
            NX = WIN(3) - WIN(1) + 1
            NY = WIN(4) - WIN(2) + 1
            POTERR = 50
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP),
     *         OFBLK, JERR)
            IF (JERR.NE.0) GO TO 960
            OFBLK = OFBLK + 1
            CALL MINIT ('READ', DLUN, DIND, CATBLK(KINAX),
     *         CATBLK(KINAX+1), WIN, RBUFF, JBUFSZ, OFBLK, JERR)
            IF (JERR.NE.0) GO TO 960
            DO 870 IY = 1,NY
               CALL MDISK ('READ', DLUN, DIND, RBUFF, ININD, JERR)
               IF (JERR.NE.0) GO TO 960
               DO 865 IX = 1,NX
                  ARRAY(IX,IY) = RBUFF(IX+ININD-1)
 865              CONTINUE
 870           CONTINUE
               DX(1) = RPOS(1) + 1.0 - WIN(1)
               DX(2) = RPOS(2) + 1.0 - WIN(2)
               JERR = 0
C                                       find maximum in array
            CALL CUBINT (ARRAY, DX, NX, NY, GMAX(NG), JERR)
            IF (JERR.NE.0) GO TO 960
            GPOS(1,NG) = DX(1) + WIN(1) - 1.0
            GPOS(2,NG) = DX(2) + WIN(2) - 1.0
            MSGTXT = 'Position cursor at major axis half-power of '
     *         // 'component ' // CDUM
            CALL MSGWRT (1)
            MSGTXT = 'Then hit button A, B, or C; hit D to quit'
            CALL MSGWRT (1)
            CALL TVWHER (QUAD, RPOS, IBUT, JERR)
            IF (JERR.NE.0) GO TO 960
            IF (IBUT.LT.8) THEN
               CALL IMA2MP (RPOS, RPOS)
               GWID(1,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *            (RPOS(2)-GPOS(2,NG))**2)
               IF (GWID(1,NG).GT.0.0) THEN
                  GWID(3,NG) = -RAD2DG * ATAN2 ((GPOS(1,NG)-RPOS(1)),
     *               (GPOS(2,NG)-RPOS(2)))
                  IF (GWID(3,NG).LT.-90.0) GWID(3,NG) = GWID(3,NG)+180.0
                  IF (GWID(3,NG).GT.90.0) GWID(3,NG) = GWID(3,NG)-180.0
                  END IF
               MSGTXT = 'Position cursor at minor axis half-power of '
     *            // 'component ' // CDUM
               CALL MSGWRT (1)
               MSGTXT = 'Then hit button A, B, or C; hit D to quit'
               CALL MSGWRT (1)
               CALL TVWHER (QUAD, RPOS, IBUT, JERR)
               IF (JERR.NE.0) GO TO 960
               IF (IBUT.LT.8) THEN
                  CALL IMA2MP (RPOS, RPOS)
                  GWID(2,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *               (RPOS(2)-GPOS(2,NG))**2)
                  END IF
               END IF
            END IF
         IF (IBUT.LE.7) THEN
            IF (ICHAN.GT.0) THEN
               IGR = NGRAY + ICHAN
               IF (NG.EQ.1) THEN
                  CALL YZERO (IGR, JERR)
                  IF (JERR.NE.0) THEN
                     WRITE (MSGTXT,1000) JERR, 'ZERO GRAPHICS PLANE'
                     GO TO 955
                     END IF
                  CALL YSLECT ('ONNN', IGR, 7, INBUF, JERR)
                  IF (JERR.NE.0) THEN
                     WRITE (MSGTXT,1000) JERR, 'TURN GRAPHICS PLANE ON'
                     GO TO 955
                     END IF
                  END IF
               SLOPE = TWOPI / 32.0D0
               OFFS = 0.0
               CG = COS (GWID(3,NG) * DG2RAD)
               SG = SIN (GWID(3,NG) * DG2RAD)
               DO 880 I = 1,33
                  X = GWID(1,NG) * COS (OFFS) / 2.0
                  Y = GWID(2,NG) * SIN (OFFS) / 2.0
                  RPOS(1) = Y * CG - X * SG + GPOS(1,NG)
                  RPOS(2) = X * CG + Y * SG + GPOS(2,NG)
                  CALL MP2IMA (RPOS, RPOS)
                  IXC(I) = RPOS(1) + 0.5
                  IYC(I) = RPOS(2) + 0.5
                  IXC(I) = MAX (1, MIN (MAXXTV(1), IXC(I)))
                  IYC(I) = MAX (1, MIN (MAXXTV(2), IYC(I)))
                  OFFS = OFFS + SLOPE
 880              CONTINUE
               CALL IMVECT ('ONNN', IGR, 33, IXC, IYC, RBUFF, JERR)
               IF (JERR.NE.0) THEN
                  WRITE (MSGTXT,1000) JERR,
     *               'DRAWING GAUSSIAN HALF-WIDTH'
                  GO TO 955
                  END IF
               END IF
            GO TO 860
            END IF
         END IF
      IF (IBUT.GT.7) NG = NG - 1
C                                       put adverbs
      POTERR = 0
      CALL ADVRBS ('GMAX', 'R', 4, 0, IDUM, GMAX, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ADVRBS ('GPOS', 'R', 8, 0, IDUM, GPOS, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ADVRBS ('GWIDTH', 'R', 12, 0, IDUM, GWID, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ADVRBS ('NGAUSS', 'I', 1, 0, NG, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      GO TO 960
C-----------------------------------------------------------------------
C                                       close image
 955  CALL MSGWRT (8)
 960  CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATIMG, .FALSE.,
     *   INBUF, JERR)
      GO TO 985
C                                       normal TV close
 970  CALL TVCLOS (CATBLK, JERR)
      GO TO 999
C                                       error handling
 980  CALL MSGWRT (8)
 985  CALL TVCLOS (CATBLK, JERR)
 990  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AU5 ERROR',I4,' ON ',A)
 1100 FORMAT ('AU5: TVINIT ERROR =',I6)
 1410 FORMAT ('REQUESTED COLOR',I7,' ILLEGAL')
 1610 FORMAT ('AU5:',I6,',',I5,' NOT IN ANY IMAGE')
 1620 FORMAT ('IMXY: ',2F8.2)
 1625 FORMAT ('IMPOS ERROR',I3,' CONVERTING TO SKY COORDINATES')
 1670 FORMAT ('Got(1)  user=',I4,'  disk=',I2,'  type= ',A2)
 1750 FORMAT ('GRAPHICS CHANNEL',I4,' NOT IN RANGE 0 -',I2)
 1760 FORMAT ('Graphics channel',I2,': red =',F6.3,' green =',F6.3,
     *   ' blue =',F6.3)
 1761 FORMAT ('Cursor color: red =',F6.3,' green =',F6.3,' blue =',
     *   F6.3)
 1770 FORMAT ('COLORS',3F8.3,' OUT OF RANGE 0.0 - 1.0')
 1850 FORMAT (A,' = ',7F8.2)
 1860 FORMAT (I1)
      END
