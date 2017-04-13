       SUBROUTINE AU7A (BRANCH)
C-----------------------------------------------------------------------
C! verbs to put/get header values, to put values into images or tables
C# POPS-appl Map-appl Header  Ext-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2005-2006, 2009
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
C   AU7A verbs:
C   Inputs:
C      BRANCH  I  =1 ADDBEAM  adds beam parameters to header
C                 =2 PUTHEAD  puts keyword-indicated value in header
C                 =3 GETHEAD  gets keyword-indicated value from header
C                 =4 PUTVALUE puts a value into an image
C                 =5 GETTHEAD reads a value from a table header
C                 =6 PUTTHEAD puts a value into a table header
C                 =7 TABGET   Read an entry from a table
C                 =8 TABPUT   Write an entry to a table
C                 =9 ACTNOISE a puthead of PIXSTD as keyword ACTNOISE
C                 =10 EGETHEAD is GETHEAD but returns ERROR not error
C   Common: (though adverbs in common)
C      INNAME   H*12  Image name.  If blank, the first match consistent
C                     with the other parameters is used.
C      INCLASS  H*6   Image class.  If blank, the first match is used.
C      INSEQ    R     Image sequence number. If zero, the first match
C                     consistent with the other parameters is used.
C      INDISK   R     the catalog resides on this disk.  If zero, all
C                     disk volumes are searched until a match is found.
C   (ADDBEAM only)
C      BMAJ     R     Beam major axis in arc sec.
C      BMIN     R     Beam minor axis in arc sec.
C      BPA      R     Beam position angle in arc sec.
C   (GETHEAD, PUTHEAD, GETTHEAD, PUTTHEAD, TABGET, TABPUT)
C      KEYWORD  H*8   FITS keyword corresponding to a header value
C      KEYVALUE R(2)  Numeric value out/in
C      KEYSTRNG H*16  String value out/in
C   (GETTHEAD, PUTTHEAD, TABGET, TABPUT)
C      INEXT     H*2  Table type (e.g. AN, SU)
C      INVER     R    the version number of the table.
C      XKEYTY    H*4  Keyword data type ('D','R','I','C','L')
C   (PUTVALUE, TABGET, TABPUT only)
C      PIXXY    R(7)  Pixel position.
C      PIXVAL   R     Pixel value.
C      OPCODE   R     'BLNK' put blanked pixel value instead.
C   (ACTNOISE)
C      PIXSTD   R     Actual rms of image
C      PIXAVG   R     Actual zero level of image
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NWPUT, NWGET
      PARAMETER (NWPUT = 76)
      PARAMETER (NWGET = 92)
C
      CHARACTER LOCNAM*12, LOCCLS*6, LOCTYP*2, STATUS*4, PRGNAM*6,
     *   HILINE*72, PHNAME*48, XVER(2)*4, CDUM*1, K1(NWPUT)*8,
     *   K2(NWGET-NWPUT)*8, KWORD(NWGET)*8, THEWRD*8, STVALU*16, OPER*4,
     *   DATYPE*4
      INTEGER   POTERR, IUSER, IVOL, LOCSEQ, ISLOT, IBUFF2(256), IERR,
     *   IHLUN, IERH, NBYT, KT, IP, IPOFF, I, IK, KPNTR(62), KBP, J,
     *   KTYPE(NWGET), POINT(NWGET), IKL(10), IC, NC, NMAX, IPIX(7),
     *   DLUN, DIND, NBUFF, NSPR, NRPS, IPOS, IBLKOF, NSPPL, NPLANE,
     *   ITRIM, ITEMP, IDUM, LBRANC
      REAL     OLDVAL, RBUFF(512), AVALUE(2), RDUM, KEYVAL(2), PIXV,
     *   PIXA
      DOUBLE PRECISION D
      LOGICAL   SAVE, EQUAL, T, LTEMP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ RBUFF, IBUFF2
      EQUIVALENCE (KWORD(1), K1(1)),    (KWORD(NWPUT+1), K2(1))
      EQUIVALENCE (KPNTR(1), KHOBJ)
      DATA XVER /'old ','new '/
      DATA DLUN /16/
      DATA IHLUN, SAVE, T /27, .TRUE.,.TRUE./
      DATA PRGNAM /'AU7A  '/
C                                       PUT and GET keywords
      DATA K1 /'OBJECT  ','TELESCOP','INSTRUME','OBSERVER','DATE-OBS',
     *   'DATE-MAP', 'BUNIT   ', 'CTYPE1  ', 'CTYPE2  ', 'CTYPE3  ',
     *   'CTYPE4  ', 'CTYPE5  ', 'CTYPE6  ', 'CTYPE7  ', 'CRVAL1  ',
     *   'CRVAL2  ', 'CRVAL3  ', 'CRVAL4  ', 'CRVAL5  ', 'CRVAL6  ',
     *   'CRVAL7  ', 'CDELT1  ', 'CDELT2  ', 'CDELT3  ', 'CDELT4  ',
     *   'CDELT5  ', 'CDELT6  ', 'CDELT7  ', 'CRPIX1  ', 'CRPIX2  ',
     *   'CRPIX3  ', 'CRPIX4  ', 'CRPIX5  ', 'CRPIX6  ', 'CRPIX7  ',
     *   'CROTA1  ', 'CROTA2  ', 'CROTA3  ', 'CROTA4  ', 'CROTA5  ',
     *   'CROTA6  ', 'CROTA7  ', 'EPOCH   ', 'DATAMAX ', 'DATAMIN ',
     *   'PRODUCT ', 'NITER   ', 'BMAJ    ', 'BMIN    ', 'BPA     ',
     *   'VELREF  ', 'ALTRVAL ', 'ALTRPIX ', 'OBSRA   ', 'OBSDEC  ',
     *   'RESTFREQ', 'XSHIFT  ', 'YSHIFT  ', 'PTYPE1  ', 'PTYPE2  ',
     *   'PTYPE3  ', 'PTYPE4  ', 'PTYPE5  ', 'PTYPE6  ', 'PTYPE7  ',
     *   'PTYPE8  ', 'PTYPE9  ', 'PTYPE10 ', 'PTYPE11 ', 'PTYPE12 ',
     *   'PTYPE13 ', 'PTYPE14 ', 'SORTORD ', 'IMGTYPE ', 'XPOFF   ',
     *   'YPOFF   '/
C                                       Keywords for GET only
      DATA K2 /'BLANK   ','IMNAME  ','IMCLASS ','IMSEQ   ', 'GCOUNT  ',
     *   'PCOUNT  ', 'NAXIS   ', 'NAXIS1  ', 'NAXIS2  ', 'NAXIS3  ',
     *   'NAXIS4  ', 'NAXIS5  ', 'NAXIS6  ', 'NAXIS7  ', 'USERNO  ',
     *   'IMTYPE  '/
C                                       number of chars + 10 *
C                                       2=Number
C                                       3=String
      DATA KTYPE /       36,38,38,38,38, 38,35,36,36,36, 36,36,36,36,26,
     *   26,26,26,26,26, 26,26,26,26,26, 26,26,26,26,26, 26,26,26,26,26,
     *   26,26,26,26,26, 26,26,25,27,27, 27,25,24,24,23, 26,27,27,25,26,
     *   28,26,26,36,36, 36,36,36,36,36, 36,36,37,37,37, 37,37,37,27,25,
     *   25,
     *   25,36,37,25,26, 26,25,26,26,26, 26,26,26,26,26, 36/
C                                       10000*nbytes + 100*offset +
C                                       position of pointer in common
      DATA POINT / 80001, 80002, 80003, 80004, 80005, 80006,
     *   80007, 80009, 80109, 80209, 80309, 80409, 80509, 80609,
     *   80029, 80129, 80229, 80329, 80429, 80529, 80629, 40010, 40110,
     *   40210, 40310, 40410, 40510, 40610, 40011, 40111, 40211, 40311,
     *   40411, 40511, 40611, 40012, 40112, 40212, 40312, 40412, 40512,
     *   40612, 40013, 40014, 40015, 20044, 20035, 40020, 40021, 40022,
     *   20045, 80033, 40023, 80030, 80031, 80032, 40024, 40025, 80008,
     *   80108, 80208, 80308, 80408, 80508, 80608, 80708, 80808, 80908,
     *   81008, 81108, 81208, 81308, 20044, 20058, 40059, 40060,
     *   40016,120017, 60218, 20042, 20034, 20039, 20040, 20041,
     *   20141, 20241, 20341, 20441, 20541, 20641, 20043, 20919/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
      LBRANC = BRANCH
C                                       Set initial values.
      POTERR = 0
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
      IF (ERRNUM.NE.0) GO TO 980
      IUSER = NLUSER
      ISLOT = 1
      LOCTYP = ' '
      IF (LBRANC.EQ.4) LOCTYP = 'MA'
      CALL CATDIR ('SRNH', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   IUSER, STATUS, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         POTERR = 101
         GO TO 980
         END IF
C                                       Choose function.
 25   GO TO (100, 200, 200, 400, 500, 500, 550, 550, 200, 200), LBRANC
C-----------------------------------------------------------------------
C                                       ADDBEAM
C                                       add beam parameters to header
C-----------------------------------------------------------------------
 100  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
C                                       Open history
 110  CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF2, IERH)
      IERR = IERH
C                                       Old beam to HI
      IF ((CATR(KRBMJ).GT.0.) .OR. (CATR(KRBMN).GT.0.) .OR.
     *   (CATR(KRBPA).NE.0.)) THEN
         WRITE (HILINE,1110) CATR(KRBMJ), CATR(KRBMN), CATR(KRBPA),
     *      XVER(1)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         END IF
C                                       New beam to HI
 120  CALL ADVERB ('BMAJ', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CATR(KRBMJ) = RDUM / 3600.0
      CALL ADVERB ('BMIN', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CATR(KRBMN) = RDUM / 3600.0
      CALL ADVERB ('BPA', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CATR(KRBPA) = RDUM
      WRITE (HILINE,1110) CATR(KRBMJ), CATR(KRBMN), CATR(KRBPA),
     *   XVER(2)
      IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
C                                       Close history file.
      IF (IERR.EQ.0) CALL HICLOS (IHLUN, SAVE, IBUFF2, IERR)
C                                       Save header
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
      IF (POTERR.EQ.0) GO TO 980
         WRITE (MSGTXT,1120) IERR
         GO TO 970
C-----------------------------------------------------------------------
C                                       PUTHEAD, GETHEAD
C                                       Change a header value
C-----------------------------------------------------------------------
C                                       Find keyword in lists
 200  IF (LBRANC.NE.9) THEN
         CALL ADVERB ('KEYWORD', 'C', 1, 8, IDUM, RDUM, THEWRD)
         IF (ERRNUM.NE.0) GO TO 980
C                                       KEYTYPE
         CALL ADVERB ('KEYTYPE', 'C', 1, 4, IDUM, RDUM, DATYPE)
         IF (ERRNUM.NE.0) GO TO 980
C                                       KEYSTRING
         CALL ADVERB ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
         IF (ERRNUM.NE.0) GO TO 980
C                                       Insert keyword in header
         CALL ADVERB ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (BRANCH.EQ.10) THEN
            LBRANC = 3
            CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, -1.0, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
C                                       ACTNOISE
      ELSE
         LBRANC = 2
         THEWRD = 'ACTNOISE'
         DATYPE = 'R'
         STVALU = ' '
         KEYVAL(2) = 0
         CALL ADVERB ('PIXSTD', 'R', 1, 0, IDUM, KEYVAL(1), CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('PIXAVG', 'R', 1, 0, IDUM, PIXA, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      NC = ITRIM (THEWRD)
      IP = 0
      NMAX = NWPUT
      IF (LBRANC.EQ.3) THEN
         NMAX = NWGET
         IF ((THEWRD(:6).EQ.'EXTVER') .OR. (THEWRD(:6).EQ.'EXTYPE'))
     *      GO TO 315
         END IF
      IF (NC.GT.0) THEN
         DO 205 IK = 1,NWGET
            IC = MOD (KTYPE(IK), 10)
            IF (NC.LE.IC) THEN
               IF (THEWRD(1:NC).EQ.KWORD(IK)(1:NC)) THEN
C                                       Found match
                  IF (IC.EQ.NC) THEN
                     GO TO 215
C                                       Header keyword name longer than
C                                       KEYWORD - save up to 10 possible
C                                       matches.
                  ELSE
                     IP = IP + 1
                     IKL(IP) = IK
                     IF (IP.EQ.10) GO TO 210
                     END IF
                  END IF
               END IF
 205        CONTINUE
         END IF
 210  IK = IKL(1)
C                                       One match
      IF (IP.EQ.1) GO TO 215
C                                       No match write as header
C                                       keyword/value pair
      IF ((IP.LE.0) .AND. (LBRANC.EQ.2)) THEN
         CALL PUTKEY (IVOL, ISLOT, THEWRD, DATYPE, KEYVAL, STVALU(:8),
     *      IERR)
         IF (IERR.NE.0) THEN
            POTERR = 33
            GO TO 980
            END IF
         IF (BRANCH.EQ.9) THEN
            THEWRD = 'ACTMEAN'
            KEYVAL(1) = PIXA
            CALL PUTKEY (IVOL, ISLOT, THEWRD, DATYPE, KEYVAL,
     *         STVALU(:8), IERR)
            IF (IERR.NE.0) THEN
               POTERR = 33
               GO TO 980
               END IF
            END IF
         GO TO 980
         END IF
C                                       Multiple possible matches
      IF (IP.GT.1) THEN
C                                       Tell possibilities
         J = MIN (5, IP)
         WRITE (MSGTXT,1210) (KWORD(IKL(I)), I = 1,J)
         CALL MSGWRT (8)
         IF (J.LT.IP) THEN
            WRITE (MSGTXT,1210) (KWORD(IKL(I)), I = 6,IP)
            CALL MSGWRT (8)
            END IF
C                                       Bail out
            POTERR = 101
            GO TO 980
         END IF
 213  IF (LBRANC.EQ.3) GO TO 300
 214  IF (BRANCH.NE.10) THEN
         WRITE (MSGTXT,1213) THEWRD
         IF (IP.EQ.1) WRITE (MSGTXT,1214) THEWRD
         CALL MSGWRT (8)
         POTERR = 101
      ELSE
         POTERR = 0
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, 1.0, CDUM)
         END IF
      GO TO 980
C                                       Read Catblk, set status
 215  THEWRD = KWORD(IK)
      IF (LBRANC.EQ.3) GO TO 315
C                                       Ok for PUT ?
      IF (IK.LE.NWPUT) GO TO 218
         IF (KWORD(IK).EQ.'USERNO') THEN
            KT = KEYVAL(1) + 0.001
            IF (KT.EQ.NLUSER) GO TO 218
            END IF
C                                       No
         GO TO 213
 218  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 220
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
C                                       Set pointers
 220  NBYT = POINT(IK) / 10000
      KT = KTYPE(IK) / 10
      IP = POINT(IK) - 10000 * NBYT
      IPOFF = IP / 100
      IP = IP - 100 * IPOFF
      IP = KPNTR(IP)
C                                       Open history
      CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF2, IERH)
      IERR = IERH
C                                       Old, new values to HI
C                                       Regular strings
      IF (KT.NE.3) GO TO 230
         IPOFF = IPOFF * ((NBYT+3) / 4)
         CALL H2CHR (NBYT, 1, CATH(IP+IPOFF), STVALU)
         WRITE (HILINE,1220) THEWRD, STVALU(1:NBYT), XVER(1)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         I = MIN (NBYT, 8)
         CALL ADVERB ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (HILINE,1220) THEWRD, STVALU(1:NBYT), XVER(2)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         CALL CHR2H (I, STVALU(1:I), 1, CATH(IP+IPOFF))
         GO TO 250
C                                       Numeric parameters
 230  CALL POPSRD ('R2D', KEYVAL, D)
      IF (NBYT.NE.2) GO TO 235
         I = D + SIGN (0.5D0, D)
         WRITE (HILINE,1230) THEWRD, CATBLK(IP+IPOFF), XVER(1)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         CATBLK(IP+IPOFF) = I
         WRITE (HILINE,1230) THEWRD, CATBLK(IP+IPOFF), XVER(2)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         GO TO 250
 235  IF (NBYT.NE.4) GO TO 240
         WRITE (HILINE,1235) THEWRD, CATR(IP+IPOFF), XVER(1)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         CATR(IP+IPOFF) = D
         WRITE (HILINE,1235) THEWRD, CATR(IP+IPOFF), XVER(2)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         GO TO 250
 240  IF (NBYT.NE.8) GO TO 250
         WRITE (HILINE,1240) THEWRD, CATD(IP+IPOFF), XVER(1)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         CATD(IP+IPOFF) = D
         WRITE (HILINE,1240) THEWRD, CATD(IP+IPOFF), XVER(2)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         GO TO 250
C                                       Close history file.
 250  IF (IERR.EQ.0) CALL HICLOS (IHLUN, SAVE, IBUFF2, IERH)
C                                       Save header
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
      IF (POTERR.EQ.0) GO TO 980
         WRITE (MSGTXT,1120) IERR
         GO TO 970
C-----------------------------------------------------------------------
C                                       GETHEAD
C                                       Read header value
C-----------------------------------------------------------------------
C                                       Look for keyword in CB keywords
 300  ITEMP = 1
      IF (BRANCH.EQ.10) MSGSUP = 32000
      CALL CATKEY ('READ', IVOL, ISLOT, THEWRD, ITEMP, IPOFF, AVALUE,
     *   IK, IBUFF2, IERR)
      MSGSUP = 0
      IF (IERR.NE.0) GO TO 214
      IF (IK.EQ.1) THEN
         CALL RCOPY (NWDPDP, AVALUE, D)
         CALL POPSRD ('D2R', KEYVAL, D)
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
      ELSE IF (IK.EQ.2) THEN
         KEYVAL(1) = AVALUE(1)
         KEYVAL(2) = 0.
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
      ELSE IF (IK.EQ.3) THEN
         STVALU = ' '
         CALL H2CHR (8, 1, AVALUE, STVALU)
         CALL ADVRBS ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
      ELSE IF (IK.EQ.4) THEN
         CALL RCOPY (1, AVALUE, IPOFF)
         KEYVAL(1) = IPOFF
         KEYVAL(2) = 0.
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
      ELSE IF (IK.EQ.5) THEN
         CALL RCOPY (1, AVALUE, LTEMP)
         KEYVAL(1) = -1.0
         IF (LTEMP) KEYVAL(1) = 1.0
         KEYVAL(2) = 0.
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C                                       Read Catblk, set status
 315  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'READ', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. ((IERR.LT.6) .OR. (IERR.GT.9))) THEN
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
         END IF
C                                       Set pointers
      CALL FXHDEX (CATBLK)
      IF ((THEWRD(:6).EQ.'EXTVER') .OR. (THEWRD(:6).EQ.'EXTYPE')) THEN
         KBP = 1
         CALL GETNUM (THEWRD(7:8), 2, KBP, D)
         IF (ERRNUM.NE.0) GO TO 980
         IPOFF = D - 0.99
         POTERR = 27
         IF ((IPOFF.LT.0) .OR. (IPOFF.GE.KIEXTN)) GO TO 970
         POTERR = 0
         NBYT = 2
         IF (THEWRD(:6).EQ.'EXTVER') THEN
            KT = 2
            IP = KIVER
         ELSE IF (THEWRD(:6).EQ.'EXTYPE') THEN
            KT = 3
            IP = KHEXT
            END IF
      ELSE
         NBYT = POINT(IK) / 10000
         KT = KTYPE(IK) / 10
         IP = POINT(IK) - 10000 * NBYT
         IPOFF = IP / 100
         IP = IP - 100 * IPOFF
         IP = KPNTR(IP)
         END IF
C                                       Strings
      IF (KT.EQ.3) THEN
         STVALU = ' '
C                                       IMCLASS, IMTYPE
         IF ((KWORD(IK).EQ.'IMCLASS') .OR. (KWORD(IK).EQ.'IMTYPE')) THEN
            IPOFF = NBYT * IPOFF + 1
            CALL H2CHR (NBYT, IPOFF, CATH(IP), STVALU)
         ELSE
            IPOFF = IPOFF * ((NBYT+3) / 4)
            CALL H2CHR (NBYT, 1, CATH(IP+IPOFF), STVALU)
            END IF
         CALL ADVRBS ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
C                                       Numeric parameters
      ELSE
         IF (NBYT.EQ.2) D = CATBLK(IP+IPOFF)
         IF (NBYT.EQ.4) D = CATR(IP+IPOFF)
         IF (NBYT.EQ.8) D = CATD(IP+IPOFF)
         CALL POPSRD ('D2R', KEYVAL, D)
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, KEYVAL, CDUM)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
C                                       Clear status
 350  CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   IUSER, 'CLRD', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
      GO TO 980
C-----------------------------------------------------------------------
C                                       PUTVALUE
C                                       add pixel value to image
C-----------------------------------------------------------------------
C                                       open map file
 400  CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 410
         WRITE (MSGTXT,1100) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1101)
         POTERR = 33
         GO TO 970
C                                       Check pixel position
 410  CALL ADVERB ('PIXXY', 'I', 7, 0, IPIX, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      DO 420 I = 1,KICTPN
         IF ((CATBLK(KIDIM).GE.I) .AND. (CATBLK(KINAX+I-1).GT.1))
     *      THEN
            IF ((IPIX(I).LT.1) .OR. (IPIX(I).GT.CATBLK(KINAX+I-1))) THEN
               POTERR = 32
               WRITE (MSGTXT,1415)
               GO TO 490
               END IF
         ELSE
            IPIX(I) = 1
            END IF
 420     CONTINUE
C                                       Open map file
      CALL ZPHFIL ('MA', IVOL, ISLOT, 1, PHNAME, IERR)
      CALL ZOPEN (DLUN, DIND, IVOL, PHNAME, T, T, T, IERR)
      IF (IERR.EQ.0) GO TO 425
         POTERR = 50
         WRITE (MSGTXT,1420) IERR
         GO TO 490
C                                       Read old pixel value
 425  NBUFF = 2 * CATBLK(KINAX)
      NRPS = NBPS / NBUFF
      IF (NRPS.LT.1) NRPS = 1
      NSPR = (NBUFF - 1) / NBPS + 1
      NSPPL = (CATBLK(KINAX+1) - 1) / NRPS + 1
      NSPPL = NSPR * NSPPL
      NPLANE = 0
      I = CATBLK(KIDIM)
      IF (I.LE.2) GO TO 435
      NPLANE = IPIX(I) - 1
 430  I = I - 1
      IF (I.LE.2) GO TO 435
         NPLANE = NPLANE * CATBLK(KINAX+I-1) + IPIX(I) - 1
         GO TO 430
 435  IPOS = ((IPIX(1)-1) * 2) / NBPS
      IBLKOF = NSPPL * NPLANE + ((IPIX(2)-1)/NRPS) * NSPR + 1 + IPOS
      IPOS = IPIX(1) - IPOS * NBPS / 2 +
     *   CATBLK(KINAX) * MOD (IPIX(2)-1, NRPS)
      CALL ZMIO ('READ', DLUN, DIND, IBLKOF, NBPS, RBUFF, 1, IERR)
      IF (IERR.EQ.0) CALL ZWAIT (DLUN, DIND, 1, IERR)
      IF (IERR.NE.0) GO TO 480
      OLDVAL = RBUFF(IPOS)
      CALL ADVERB ('PIXVAL', 'R', 1, 0, IDUM, PIXV, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      RBUFF(IPOS) = PIXV
      CALL ADVERB ('OPCODE', 'C', 1, 4, IDUM, RDUM, STATUS)
      IF (ERRNUM.NE.0) GO TO 980
      EQUAL = STATUS.EQ.'BLNK'
      IF (EQUAL) RBUFF(IPOS) = FBLANK
      CALL ZMIO ('WRIT', DLUN, DIND, IBLKOF, NBPS, RBUFF, 1, IERR)
      IF (IERR.EQ.0) CALL ZWAIT (DLUN, DIND, 1, IERR)
      IF (IERR.NE.0) GO TO 480
      CALL ZCLOSE (DLUN, DIND, IERR)
C                                       Update header?
      IF ((EQUAL) .AND. (CATR(KRBLK).EQ.FBLANK)) GO TO 460
      IF ((.NOT.EQUAL) .AND. (PIXV.GE.CATR(KRDMN)) .AND.
     *   (PIXV.LE.CATR(KRDMX))) GO TO 460
         IF (EQUAL) CATR(KRBLK) = FBLANK
         IF ((.NOT.EQUAL) .AND. (PIXV.LT.CATR(KRDMN))) CATR(KRDMN) =
     *      PIXV
         IF ((.NOT.EQUAL) .AND. (PIXV.GT.CATR(KRDMX))) CATR(KRDMX) =
     *      PIXV
         CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'REST', IBUFF2, IERR)
         IF (IERR.EQ.0) GO TO 460
            WRITE (MSGTXT,1425) IERR
            CALL MSGWRT (8)
C                                       Open history
 460  CALL ADVERB ('DOHIST', 'R', 1, 0, IDUM,  RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (RDUM.LT.-1.5) GO TO 495
         CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF2, IERH)
         IF (IERH.NE.0) GO TO 470
            J = CATBLK(KIDIM)
            WRITE (HILINE,1460) IPIX
            CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
            IF (IERH.NE.0) GO TO 470
            IF (OLDVAL.NE.FBLANK) WRITE (HILINE,1461) XVER(1), OLDVAL
            IF (OLDVAL.EQ.FBLANK) WRITE (HILINE,1462) XVER(1)
            CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
            IF (IERH.NE.0) GO TO 470
            IF (.NOT.EQUAL) WRITE (HILINE,1461) XVER(2), PIXV
            IF (EQUAL) WRITE (HILINE,1462) XVER(2)
            CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
 470     CALL HICLOS (IHLUN, SAVE, IBUFF2, IERH)
         GO TO 495
C                                       Map IO error
 480  WRITE (MSGTXT,1480) IERR
      CALL MSGWRT (8)
      CALL ZCLOSE (DLUN, DIND, IERR)
      IERR = 0
      GO TO 495
C                                       error
 490  CALL MSGWRT (8)
C                                       Clear status
 495  CALL CATDIR ('CSTA', IVOL, ISLOT, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   IUSER, 'CLWR', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
      GO TO 980
C-----------------------------------------------------------------------
C                                       GETTHEAD, PUTTHEAD
C                                       Read/write table keyword value
C                                       pairs.
C-----------------------------------------------------------------------
 500  OPER = 'READ'
      IF (LBRANC.EQ.6) OPER = 'WRIT'
      CALL TBHEAD (OPER, IVOL, ISLOT, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       TABGET, TABPUT
C                                       Read/write table entries.
C-----------------------------------------------------------------------
 550  OPER = 'READ'
      IF (LBRANC.EQ.8) OPER = 'WRIT'
      CALL TBDATA (OPER, IVOL, ISLOT, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       Print error.
 970  CALL MSGWRT (8)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('CATALOG ENTRY NOT FOUND')
 1100 FORMAT ('CATALOG READ ERROR',I5)
 1101 FORMAT ('MAP TOO BUSY')
 1110 FORMAT ('ADDBEAM BMAJ=',1PE13.5,' BMIN=',1PE13.5,' BPA=',0PF8.3,
     *   ' / ',A3)
 1120 FORMAT ('CATALOG WRITE ERROR',I5)
 1210 FORMAT ('COULD BE:',5(2X,A8))
 1213 FORMAT ('KEYWORD =''',A8,''' NOT RECOGNIZED')
 1214 FORMAT ('KEYWORD =''',A8,''' ALLOWED ONLY FOR GETHEAD')
 1220 FORMAT ('PUTHEAD ',A8,' =''',A,'''',9X,'/ ',A3)
 1230 FORMAT ('PUTHEAD ',A8,' =',I9,10X,'/ ',A3)
 1235 FORMAT ('PUTHEAD ',A8,' =',1PE13.5,6X,'/ ',A3)
 1240 FORMAT ('PUTHEAD ',A8,' =',1PE17.9,2X,'/ ',A3)
 1245 FORMAT ('PUTHEAD ',A8,' =',I11,8X,'/ ',A3)
 1410 FORMAT ('PUTVALUE WORKS ONLY ON FLOATING IMAGES: USE CNVRT')
 1415 FORMAT ('PIXXY OUT OF RANGE OR NOT ON PIXEL')
 1420 FORMAT ('ERROR',I5,' OPENING IMAGE FILE')
 1425 FORMAT ('UNABLE TO UPDATE HEADER FOR IMAGE CHANGES: ERROR',I5)
 1460 FORMAT ('PUTVALUE PIXXY=',I7,6(',',I6))
 1461 FORMAT ('PUTVALUE / ',A3,' value=',1PE14.6)
 1462 FORMAT ('PUTVALUE / ',A3,' value blanked')
 1480 FORMAT ('ERROR',I5,' DOING IMAGE IO')
      END
