LOCAL INCLUDE 'PRTPL.INC'
      REAL   XMWPF, XWPL, XWPP, SCALEX, SCALEY
      INTEGER   IXL, IYL, IX0, IY0, NGRAYS, IRRN(2), INBLK,
     *   IMAXS, IMIDS, IYSPAC, IXBIT, LINWID
      COMMON /CPRT/ SCALEX, SCALEY, IXL, IYL, IX0, IY0, NGRAYS,
     *   IRRN, INBLK, IMAXS, IMIDS, XMWPF, XWPL, XWPP, IYSPAC,
     *   IXBIT, LINWID
LOCAL END
      PROGRAM PRTPL
C-----------------------------------------------------------------------
C! Task to write a plot file to a particular printer-plotter
C# Graphics Plot-appl Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2009, 2015
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
C   PRTPL is a program for the graphics package. The program runs as a
C   detached task initiated from AIPS.  First a cataloged file is found
C   using data passed from AIPS.  The list of associated files is
C   searched for a PLot file corresponding to the version number.  The
C   graphics commands in this file are used to create a bit map on disk.
C   This bit map is then used to write to a particular printer-plotter
C   by ZDOPRT.
C   INPUTS:  (from AIPS)
C            INNAME  R(3)   Name of primary file.
C            INCLASS R(2)   Class of primary file.
C            INSEQ   R      Sequence number of primary file.
C            INDISK  R      Disk volume number. 0 means try all.
C            INVERS  R      Extension file version number. 0 means.
C                           use the highest version number.
C            ASPMM   R      Arc sec per milimetersec, 0=self scale.
C                           on normal CNTR, GREYS, PCNTR plots
C                           else used as scale factor (0 < aspmm < 1)
C            COPIES   R     Number of copies of the plot.
C            DOCENTER R     TRUE (1) means center on a page. FALSE (-1)
C                           means use maximum possible plot even if the
C                           plot runs across a page boundary.
C            DOINVERS R     TRUE (>= 0) means white on black image
C                           normal is black on white
C            BADDISK  R(10)    Disks to avoid for scratch
C-----------------------------------------------------------------------
      CHARACTER NAMIN*12, CLSIN*6, TYPIN*2, STATUS*4, NAME*6,
     *   GFILE*48
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL   CENTER, DSKIN, SEQIN, ASPMM, VERSN, XINVER, COPIES,
     *   BADD(10)
      INTEGER   BUFFER(256), SCRTCH(8192), CATERR, LUN, FIND, IERR,
     *   RETCOD, SLOT, IVER, ISEQ, IERR2, USER, ISIZE, ICENT, NCOPY,
     *   IROUND, IVOL, I, NPARMS
      LOGICAL   NOEXCL, WAIT, EQUAL, NOMAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (NOMAP, NOEXCL)
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, ASPMM,
     *   COPIES, CENTER, XINVER, BADD
      DATA NAME /'PRTPL '/
      DATA NOEXCL, WAIT /.FALSE.,.TRUE./
      DATA LUN, ISIZE /26, 8192/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                       Get input values from AIPS.
      NPARMS = 22
      CALL GTPARM (NAME, NPARMS, RQUICK, XNAMIN, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C
      RETCOD = IERR
      IF (RQUICK) CALL RELPOP (RETCOD, BUFFER, IERR2)
      IF (IERR.NE.0) GO TO 995
      RETCOD = 1
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      TYPIN = ' '
C                                       Get map header.
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IVER = IROUND (VERSN)
      DO 30 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 30      CONTINUE
      IF (CENTER.LE.0.0) ICENT = -1
      IF (CENTER.GT.0.0) ICENT = 1
      IF (XINVER.GT.0.0) IXBIT = 0
      IF (XINVER.LE.0.0) IXBIT = 1
      USER = NLUSER
      SLOT = 1
      CALL CATDIR ('SRCH', IVOL, SLOT, NAMIN, CLSIN, ISEQ, TYPIN, USER,
     *   STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Read catalog header.
      CALL CATIO ('READ', IVOL, SLOT, CATBLK, 'REST', BUFFER, CATERR)
      IF ((CATERR.EQ.0) .OR. (CATERR.EQ.6)) GO TO 50
 45      WRITE (MSGTXT,1045) IERR
         CALL MSGWRT (8)
         GO TO 995
C                                        Find plot file:
C                                        if PL & IVER=0, then main file
 50   EQUAL = TYPIN .EQ. 'PL'
      IF ((EQUAL) .AND. (CATERR.EQ.6)) GO TO 45
      IF ((EQUAL) .AND. (IVER.EQ.0)) IVER = CATBLK(KIIMS)
C                                        else take IVER as given or
C                                        as max version #
      IF (IVER.LE.0) CALL FNDEXT ('PL', CATBLK, IVER)
C                                       PLot file not found.
      IF (IVER.LE.0) THEN
         WRITE (MSGTXT,1060)
         CALL MSGWRT (8)
         GO TO 995
         END IF
C
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, SLOT, IVER, GFILE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open graphics file.
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, NOMAP, NOEXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070)
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Write bit map & que to printer.
      NCOPY = IROUND (COPIES)
      IF (NCOPY.LT.1) NCOPY = 1
      CALL PRTDRW (LUN, FIND, IVOL, ICENT, ISIZE, ASPMM, NCOPY, SCRTCH,
     *   IERR)
      IF (IERR.EQ.0) THEN
         RETCOD = 0
      ELSE
         WRITE (MSGTXT,1080)
         CALL MSGWRT (8)
         END IF
C                                       Close graph file.
      CALL ZCLOSE (LUN, FIND, IERR)
 995  CALL DIE (RETCOD, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1030 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1045 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
 1060 FORMAT ('PLOT FILE NOT IN CATALOG')
 1070 FORMAT ('PLOT FILE NOT FOUND')
 1080 FORMAT ('ERROR WRITING BIT MAP')
      END
      SUBROUTINE GRYRAN (VALUE, NBITS, BITS)
C-----------------------------------------------------------------------
C   GRYRAN sets bits in the bit array BITS by calling a random number
C   generator.  If the generated value is < VALUE, the bit is set on,
C   else it is set off.
C   Inputs: VALUE  R           test level (0. <= VALUE <= 1.)
C           NBITS  I           > 0 -> number of bits to set
C                              < 0 -> do initialization
C           BITS   I(NBITS)    bit array, set to 1/0's
C-----------------------------------------------------------------------
      REAL      VALUE, X
      INTEGER   NBITS, BITS(1), I
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        init
      IF (NBITS.GT.0) GO TO 10
         CALL RANDIN (I)
         IF (I.EQ.0) GO TO 999
            WRITE (MSGTXT,1000) I
            CALL MSGWRT (7)
            GO TO 999
C                                        do the usual
 10   DO 20 I = 1,NBITS
         CALL RANDUM (X)
         BITS(I) = 0
         IF (X.LT.VALUE) BITS(I) = 1
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRYRAN: Strange return from RANDIN :',I7)
      END
      SUBROUTINE PRBMCR (IVOL, ILUN, IND, INX, INY, IFILE, ISCVOL,
     *   IBLKS, WORDS, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   PRBMCR will calculate the size for a bit map file and create
C   a unique version of this scratch file on a disk other than the
C   disk used by the graphics file if possible.
C   INPUTS:   IVOL    I   disk volume number for this scratch file.
C             ILUN    I   logical unit number for this file.
C             INX     I   number of X pixels in this file. (dots)
C             INY     I   number of Y pixels in this file. (dots)
C   OUTPUTS:  IND     I   pointer to info in FTAB.
C             IFILE   C*48   file name of scratch file.
C             ISCVOL  I   Volume number of bit map file.
C             IBLKS   I   The number of 512 byte blocks in this file.
C             WORDS   R   The number of words needed for the bit map.
C             SCRTCH  I(512)     Scratch buffer
C             IERR    I   error indicator.
C                         0 = no error.
C                         1 = could not build file name.
C                         2 = unexpected create error.
C                         3 = all 99 versions of scratch in use.
C                         4 = unexpected open error.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      REAL      WORDS
      INTEGER   IERR, ILUN, IND, INX, INY, ISCVOL, ISEQ, IVER, IVOL,
     *   IBLKS, SCRTCH(*), ISIZE
      LOGICAL   EXCL, NOMAP, WAIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (EXCL, WAIT)
      DATA IVER /1/
      DATA EXCL, NOMAP /.TRUE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Calculate size of new file.
      ISIZE = ((INX-1) / NBITWD + 1)
      WORDS = ISIZE * INY
      ISIZE = (WORDS-1) / 256  +  2.0001
      IBLKS = ISIZE
C                                       Find disk on volume other than
C                                       IVOL if possible.
      NSCR = 1
      SCRVOL(1) = IVOL
      CALL SCREAT (ISIZE, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IERR = 2
         CALL MSGWRT (8)
         NSCR = 0
         GO TO 999
         END IF
C                                       Undo lie to SCREAT
      SCRVOL(1) = SCRVOL(2)
      SCRCNO(1) = SCRCNO(2)
      NSCR = 1
      ISCVOL = SCRVOL(1)
      ISEQ = SCRCNO(1)
C                                       Create scratch file name.
      CALL ZPHFIL ('SC', ISCVOL, ISEQ, IVER, IFILE, IERR)
C                                       Open scratch file.
      CALL ZOPEN (ILUN, IND, ISCVOL, IFILE, NOMAP, EXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRBMCR: SCRATCH CREATE ERROR ',I5)
 1020 FORMAT ('PRBMCR: COULD NOT OPEN FILE! ZOPEN ERR =',I5)
      END
      SUBROUTINE PRCHAR (ILEN, HORV, STRING, ILUN, IND, IOBLK, IERR)
C-----------------------------------------------------------------------
C   PRCHAR will generate the vector commands for characters.
C   INPUTS:   ILEN     I   number of characters in the character string
C             HORV     I   0 = draw characters horizontally, 1=vertical
C             STRING   C(*)   array containing ASCII characters
C             ILUN     I   logical unit number of bit map file.
C             IND      I   FTAB index of bit map file.
C             IOBLK    I(??)   I/O block for bit map file.
C   COMMON:   CSIZPR   I(2)   character size (X,Y) in bits.
C             IXL      I   current absolute x coordinate in bit map.
C             IYL      I   current absolute Y coordinate in bit map.
C   OUTPUTS:  IERR     I   error code.
C                          0 = ok.
C   COMMON:   IXL      I   updated position.
C             IYL      I   updated Y position.
C             IOBLK    I   updated I/O block.
C-----------------------------------------------------------------------
      CHARACTER IT*2, STRING*(*)
      INTEGER   IOBLK(1), HORV, I, ICHAR, IERR, ILEN, ILUN, KT,
     *   INCOM, IND, INDEX, IOP, IXC, IXSAV, IYC, IYSAV, J, K,
     *   BITS(15), NIT, JT, LT
      INTEGER   ICHTAB(931), INDTAB(95)
      INTEGER   SPACE ( 1), EXCLAM( 6), QUOTE ( 6), POUND (13)
     *        , DOLLAR(15), PERCNT(16), AMPERS(13), APOSTR( 8)
     *        , LPAREN( 6), RPAREN( 6), ASTER (13), PLUS  (07)
     *        , COMMA ( 8), MINUS ( 4), PERIOD( 7), SLASH ( 4)
     *        , ZERO  (14), ONE   ( 8), TWO   (12), THREE (16)
     *        , FOUR  ( 7), FIVE  (12), SIX   (13), SEVEN ( 8)
     *        , EIGHT (18), NINE  (13), COLON (13), SEMICO(14)
     *        , LESS  ( 5), EQUALS( 7), GREATR( 5), QUESTN(11)
      INTEGER   ATSIGN(15), AUPPER(10), BUPPER(17), CUPPER(10)
     *        , DUPPER(11), EUPPER( 9), FUPPER( 8), GUPPER(12)
     *        , HUPPER(10), IUPPER(10), JUPPER(10), KUPPER(10)
     *        , LUPPER( 5), MUPPER( 7), NUPPER(10), OUPPER(11)
     *        , PUPPER( 9), QUPPER(14), RUPPER(12), SUPPER(14)
     *        , TUPPER( 7), UUPPER( 8), VUPPER( 7), WUPPER( 7)
     *        , XUPPER(11), YUPPER( 8), ZUPPER( 8), LBRACK( 6)
     *        , BKSLSH( 4), RBRACK( 6), CARET ( 5), USCORE( 4)
     *        , ACCENT( 7), ALOWER(15), BLOWER(13), CLOWER(10)
     *        , DLOWER(13), ELOWER(12), FLOWER( 9), GLOWER(13)
     *        , HLOWER(10), ILOWER(10), JLOWER(10), KLOWER(11)
     *        , LLOWER( 8), MLOWER(14), NLOWER(10), OLOWER(11)
     *        , PLOWER(13), QLOWER(13), RLOWER( 9), SLOWER(11)
     *        , TLOWER(10), ULOWER(10), VLOWER( 7), WLOWER(14)
     *        , XLOWER( 7), YLOWER(13), ZLOWER( 6), LBRACE(14)
     *        , ORSIGN( 4), RBRACE(14), TILDE ( 6)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTPL.INC'
      EQUIVALENCE (SPACE , ICHTAB(  1)), (EXCLAM, ICHTAB(  2))
     *          , (QUOTE , ICHTAB(  8)), (POUND , ICHTAB( 14))
     *          , (DOLLAR, ICHTAB( 27)), (PERCNT, ICHTAB( 42))
     *          , (AMPERS, ICHTAB( 58)), (APOSTR, ICHTAB( 71))
     *          , (LPAREN, ICHTAB( 79)), (RPAREN, ICHTAB( 85))
     *          , (ASTER , ICHTAB( 91)), (PLUS  , ICHTAB(104))
     *          , (COMMA , ICHTAB(111)), (MINUS , ICHTAB(119))
     *          , (PERIOD, ICHTAB(123)), (SLASH , ICHTAB(130))
     *          , (ZERO  , ICHTAB(134)), (ONE   , ICHTAB(148))
     *          , (TWO   , ICHTAB(156)), (THREE , ICHTAB(168))
     *          , (FOUR  , ICHTAB(184)), (FIVE  , ICHTAB(191))
     *          , (SIX   , ICHTAB(203)), (SEVEN , ICHTAB(216))
     *          , (EIGHT , ICHTAB(224)), (NINE  , ICHTAB(242))
     *          , (COLON , ICHTAB(255)), (SEMICO, ICHTAB(268))
     *          , (LESS  , ICHTAB(282)), (EQUALS, ICHTAB(287))
     *          , (GREATR, ICHTAB(294)), (QUESTN, ICHTAB(299))
      EQUIVALENCE (ATSIGN, ICHTAB(310)), (AUPPER, ICHTAB(325))
     *          , (BUPPER, ICHTAB(335)), (CUPPER, ICHTAB(352))
     *          , (DUPPER, ICHTAB(362)), (EUPPER, ICHTAB(373))
     *          , (FUPPER, ICHTAB(382)), (GUPPER, ICHTAB(390))
     *          , (HUPPER, ICHTAB(402)), (IUPPER, ICHTAB(412))
     *          , (JUPPER, ICHTAB(422)), (KUPPER, ICHTAB(432))
     *          , (LUPPER, ICHTAB(442)), (MUPPER, ICHTAB(447))
     *          , (NUPPER, ICHTAB(454)), (OUPPER, ICHTAB(464))
     *          , (PUPPER, ICHTAB(475)), (QUPPER, ICHTAB(484))
     *          , (RUPPER, ICHTAB(498)), (SUPPER, ICHTAB(510))
     *          , (TUPPER, ICHTAB(524)), (UUPPER, ICHTAB(531))
     *          , (VUPPER, ICHTAB(539)), (WUPPER, ICHTAB(546))
     *          , (XUPPER, ICHTAB(553)), (YUPPER, ICHTAB(564))
     *          , (ZUPPER, ICHTAB(572)), (LBRACK, ICHTAB(580))
     *          , (BKSLSH, ICHTAB(586)), (RBRACK, ICHTAB(590))
     *          , (CARET , ICHTAB(596)), (USCORE, ICHTAB(601))
      EQUIVALENCE (ACCENT, ICHTAB(605)), (ALOWER, ICHTAB(612))
     *          , (BLOWER, ICHTAB(627)), (CLOWER, ICHTAB(640))
     *          , (DLOWER, ICHTAB(650)), (ELOWER, ICHTAB(663))
     *          , (FLOWER, ICHTAB(675)), (GLOWER, ICHTAB(684))
     *          , (HLOWER, ICHTAB(697)), (ILOWER, ICHTAB(707))
     *          , (JLOWER, ICHTAB(717)), (KLOWER, ICHTAB(727))
     *          , (LLOWER, ICHTAB(738)), (MLOWER, ICHTAB(746))
     *          , (NLOWER, ICHTAB(760)), (OLOWER, ICHTAB(770))
     *          , (PLOWER, ICHTAB(781)), (QLOWER, ICHTAB(794))
     *          , (RLOWER, ICHTAB(807)), (SLOWER, ICHTAB(816))
     *          , (TLOWER, ICHTAB(827)), (ULOWER, ICHTAB(837))
     *          , (VLOWER, ICHTAB(847)), (WLOWER, ICHTAB(854))
     *          , (XLOWER, ICHTAB(868)), (YLOWER, ICHTAB(875))
     *          , (ZLOWER, ICHTAB(888)), (LBRACE, ICHTAB(894))
     *          , (ORSIGN, ICHTAB(908)), (RBRACE, ICHTAB(912))
     *          , (TILDE , ICHTAB(926))
C-----------------------------------------------------------------------
      DATA SPACE  /00/
      DATA EXCLAM /02, 38, 33, 01, 30, 00/
      DATA QUOTE  /02, 28, 26, 02, 48, 46/
      DATA POUND  /02, 10, 18, 02, 58, 50, 02, 62, 02, 02
     *           , 06, 66, 00/
      DATA DOLLAR /10, 01, 51, 62, 63, 54, 14, 05, 06, 17
     *           , 67, 02, 38, 30, 00/
      DATA PERCNT /05, 07, 18, 27, 16, 07, 02, 01, 67, 05
     *           , 50, 61, 52, 41, 50, 00/
      DATA AMPERS /11, 60, 06, 07, 18, 48, 46, 02, 01, 10
     *           , 30, 63, 00/
      DATA APOSTR /06, 24, 46, 48, 38, 37, 47, 00/
      DATA LPAREN /04, 40, 22, 26, 48, 00/
      DATA RPAREN /04, 20, 42, 46, 28, 00/
      DATA ASTER  /02, 01, 67, 02, 07, 61, 02, 04, 64, 02
     *           , 37, 31, 00/
      DATA PLUS   /02, 14, 54, 02, 36, 32, 00/
      DATA COMMA  /06, 20, 42, 44, 34, 33, 43, 00/
      DATA MINUS  /02, 14, 54, 00/
      DATA PERIOD /05, 20, 30, 31, 21, 20, 00/
      DATA SLASH  /02, 01, 67, 00/
      DATA ZERO   /09, 10, 50, 61, 67, 58, 18, 07, 01, 10
     *           , 02, 01, 67, 00/
      DATA ONE    /02, 10, 50, 03, 30, 38, 16, 00/
      DATA TWO    /10, 07, 18, 58, 67, 65, 54, 24, 02, 00
     *           , 60, 00/
      DATA THREE  /07, 07, 18, 58, 67, 65, 54, 34, 06, 54
     *           , 63, 61, 50, 10, 01, 00/
      DATA FOUR   /05, 50, 58, 03, 02, 72, 00/
      DATA FIVE   /10, 01, 10, 40, 62, 63, 45, 05, 08, 68
     *           , 67, 00/
      DATA SIX    /11, 04, 54, 63, 61, 50, 10, 01, 06, 28
     *           , 58, 67, 00/
      DATA SEVEN  /06, 20, 23, 67, 68, 08, 07, 00/
      DATA EIGHT  /16, 14, 03, 01, 10, 50, 61, 63, 54, 14
     *           , 05, 07, 18, 58, 67, 65, 54, 00/
      DATA NINE   /11, 01, 10, 40, 62, 67, 58, 18, 07, 05
     *           , 14, 64, 00/
      DATA COLON  /05, 22, 32, 33, 23, 22, 05, 26, 36, 37
     *           , 27, 26, 00/
      DATA SEMICO /06, 10, 32, 34, 24, 23, 33, 05, 26, 36
     *           , 37, 27, 26, 00/
      DATA LESS   /03, 50, 14, 58, 00/
      DATA EQUALS /02, 12, 52, 02, 16, 56, 00/
      DATA GREATR /03, 10, 54, 18, 00/
      DATA QUESTN /07, 06, 07, 18, 58, 67, 34, 33, 01, 31
     *           , 00/
      DATA ATSIGN /13, 54, 45, 34, 43, 54, 64, 66, 48, 28
     *           , 06, 02, 20, 50, 00/
      DATA AUPPER /05, 00, 05, 38, 65, 60, 02, 03, 63, 00/
      DATA BUPPER /06, 00, 50, 61, 63, 54, 14, 05, 08, 58
     *           , 67, 65, 54, 02, 18, 10, 00/
      DATA CUPPER /08, 67, 58, 28, 06, 02, 20, 50, 61, 00/
      DATA DUPPER /06, 00, 40, 62, 66, 48, 08, 02, 18, 10
     *           , 00/
      DATA EUPPER /04, 60, 00, 08, 68, 02, 34, 04, 00/
      DATA FUPPER /03, 00, 08, 68, 02, 34, 04, 00/
      DATA GUPPER /10, 67, 58, 28, 06, 02, 20, 50, 61, 64
     *           , 44, 00/
      DATA HUPPER /02, 00, 08, 02, 60, 68, 02, 04, 64, 00/
      DATA IUPPER /02, 10, 50, 02, 30, 38, 02, 18, 58, 00/
      DATA JUPPER /05, 01, 10, 20, 31, 38, 02, 18, 58, 00/
      DATA KUPPER /02, 00, 08, 02, 68, 02, 02, 24, 60, 00/
      DATA LUPPER /03, 08, 00, 60, 00/
      DATA MUPPER /05, 00, 08, 35, 68, 60, 00/
      DATA NUPPER /02, 00, 08, 02, 07, 61, 02, 60, 68, 00/
      DATA OUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     *           , 00/
      DATA PUPPER /07, 00, 08, 58, 67, 66, 55, 05, 00/
      DATA QUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     *           , 02, 33, 60, 00/
      DATA RUPPER /07, 00, 08, 58, 67, 66, 55, 05, 02, 15
     *           , 60, 00/
      DATA SUPPER /12, 01, 10, 50, 61, 63, 54, 14, 05, 07
     *           , 18, 58, 67, 00/
      DATA TUPPER /02, 30, 38, 02, 08, 68, 00/
      DATA UUPPER /06, 08, 01, 10, 50, 61, 68, 00/
      DATA VUPPER /05, 08, 03, 30, 63, 68, 00/
      DATA WUPPER /05, 08, 00, 33, 60, 68, 00/
      DATA XUPPER /04, 00, 01, 67, 68, 04, 08, 07, 61, 60
     *           , 00/
      DATA YUPPER /03, 08, 35, 68, 02, 35, 30, 00/
      DATA ZUPPER /06, 08, 68, 67, 01, 00, 60, 00/
      DATA LBRACK /04, 40, 20, 28, 48, 00/
      DATA BKSLSH /02, 07, 61, 00/
      DATA RBRACK /04, 20, 40, 48, 28, 00/
      DATA CARET  /03, 05, 38, 65, 00/
      DATA USCORE /02,-01,-61, 00/
      DATA ACCENT /05, 27, 28, 38, 37, 55, 00/
      DATA ALOWER /05, 06, 26, 35, 31, 40, 07, 31, 20, 10
     *           , 01, 02, 13, 33, 00/
      DATA BLOWER /02, 08, 00, 08, 02, 20, 30, 41, 44, 35
     *           , 25, 03, 00/
      DATA CLOWER /08, 41, 30, 10, 01, 04, 15, 35, 44, 00/
      DATA DLOWER /02, 48, 40, 08, 42, 20, 10, 01, 04, 15
     *           , 25, 43, 00/
      DATA ELOWER /10, 40, 10, 01, 04, 15, 35, 44, 43, 32
     *           , 02, 00/
      DATA FLOWER /04, 10, 17, 28, 37, 02, 04, 24, 00/
      DATA GLOWER /11, 40, 10, 01, 04, 15, 35, 44,-41,-23
     *           ,-13,-02, 00/
      DATA HLOWER /02, 00, 08, 05, 03, 25, 35, 44, 40, 00/
      DATA ILOWER /01, 37, 03, 25, 35, 30, 02, 20, 40, 00/
      DATA JLOWER /01, 37, 06, 35,-32,-23,-13,-02,-01, 00/
      DATA KLOWER /02, 08, 00, 02, 01, 45, 03, 40, 22, 23
     *           , 00/
      DATA LLOWER /02, 20, 40, 03, 30, 38, 28, 00/
      DATA MLOWER /06, 00, 04, 15, 25, 34, 30, 05, 34, 45
     *           , 55, 64, 60, 00/
      DATA NLOWER /02, 00, 05, 05, 03, 25, 35, 44, 40, 00/
      DATA OLOWER /09, 01, 04, 15, 35, 44, 41, 30, 10, 01
     *           , 00/
      DATA PLOWER /02,-03, 05, 08, 03, 25, 35, 44, 41, 30
     *           , 20, 02, 00/
      DATA QLOWER /02,-43, 45, 08, 43, 25, 15, 04, 01, 10
     *           , 20, 42, 00/
      DATA RLOWER /02, 00, 05, 04, 03, 25, 35, 44, 00/
      DATA SLOWER /09, 00, 30, 41, 42, 33, 13, 04, 15, 45
     *           , 00/
      DATA TLOWER /02, 06, 26, 05, 18, 11, 20, 30, 41, 00/
      DATA ULOWER /05, 05, 01, 10, 20, 42, 02, 40, 45, 00/
      DATA VLOWER /05, 05, 02, 20, 42, 45, 00/
      DATA WLOWER /06, 05, 01, 10, 20, 31, 35, 05, 31, 40
     *           , 50, 61, 65, 00/
      DATA XLOWER /02, 00, 55, 02, 05, 50, 00/
      DATA YLOWER /05, 05, 01, 10, 30, 41, 05, 45,-42,-33
     *           ,-23,-12, 00/
      DATA ZLOWER /04, 05, 55, 00, 50, 00/
      DATA LBRACE /06, 40, 30, 21, 23, 14, 04, 05, 14, 25
     *           , 27, 38, 48, 00/
      DATA ORSIGN /02, 30, 38, 00/
      DATA RBRACE /06, 20, 30, 41, 43, 54, 64, 05, 54, 45
     *           , 47, 38, 28, 00/
      DATA TILDE  /04, 06, 28, 46, 68, 00/
C
      DATA INDTAB /  1,   2,   8,  14,  27,  42,  58,  71,  79,  85,
     *              91, 104, 111, 119, 123, 130, 134, 148, 156, 168,
     *             184, 191, 203, 216, 224, 242, 255, 268, 282, 287,
     *             294, 299, 310, 325, 335, 352, 362, 373, 382, 390,
     *             402, 412, 422, 432, 442, 447, 454, 464, 475, 484,
     *             498, 510, 524, 531, 539, 546, 553, 564, 572, 580,
     *             586, 590, 596, 601, 605, 612, 627, 640, 650, 663,
     *             675, 684, 697, 707, 717, 727, 738, 746, 760, 770,
     *             781, 794, 807, 816, 827, 837, 847, 854, 868, 875,
     *             888, 894, 908, 912, 926 /
C-----------------------------------------------------------------------
C                                       Loop for each character.
      DO 100 I = 1,ILEN
C                                       get standard ASCII char
C                                       in machine-ind. fashion
         JT = NBITWD/8
         IT(1:1) = STRING(I:I)
         CALL ZCLC8 (1, IT, JT, LT)
         CALL ZI32IL (1, 1, LT, KT)
         NIT = NBITWD - (JT-1)*8
         CALL ZGTBIT (NIT, KT, BITS)
         CALL ZPTBIT (8, ICHAR, BITS(NIT-7))
C                                       Set origin & check validity.
         ICHAR = ICHAR - 31
         IF ((ICHAR.LT.1) .OR. (ICHAR.GT.95)) ICHAR = 32
C                                       Find index into table of vectrs
         INDEX = INDTAB(ICHAR)
         IXSAV = IXL
         IYSAV = IYL
C                                       DO WHILE no. of commands not 0.
         DO 50 J = 1,99
            INCOM = ICHTAB(INDEX)
            IF (INCOM.EQ.0) GO TO 60
C                                       Loop for all vector commands.
            IOP = 3
            IXL = IXSAV
            IYL = IYSAV
            DO 30 K = 1,INCOM
               INDEX = INDEX + 1
C                                       Calc X and Y values.
               IYC = (MOD(ICHTAB(INDEX), 10)*CSIZPR(2)) / 10 + IYSAV
               IXC = (ICHTAB(INDEX)/10 * CSIZPR(1)) / 10
               IXC = ABS (IXC) + IXSAV
               CALL PRTVEC (IXC, IYC, IOP, ILUN, IND, IOBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               IOP = 4
 30            CONTINUE
            INDEX = INDEX + 1
 50         CONTINUE
C                                       Set last X and Y.
C                                       Horizontal
 60      IF (HORV.EQ.1) GO TO 65
            IYL = IYSAV
            IXL = IXSAV + CSIZPR(1)
            GO TO 100
C                                       Vertical.
 65      CONTINUE
            IXL = IXSAV
            IYL = IYSAV - CSIZPR(2) - IYSPAC
C
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PRSBIT (IX, IY, IN, ILUN, IND, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   PRSBIT will set a bit in the bit map.  This routine keeps part of
C   the bit map in two buffers.  The routine determines if the bit to be
C   set is in one of these two buffers.  If not the buffer containing
C   the section of the bit map farthest from the bit is written to disk
C   and the 'page' containing the bit is read from disk.  The bit map is
C   stored such that the top of the plot is stored in the first part of
C   the file.
C   Inputs:  IX     I(IN)   the X positions of the bits in the bit map
C                   ( 1 relative starting from bottom left corner of
C                   the finished graph.)
C            IY     I(IN)   the line ( Y positions ) of the bits to be
C                   set.
C            IN      I   the number of bits to be set.
C            ILUN   I   the logical unit number of the bit map file.
C            IND    I   index into FTAB info.
C            SCRTCH I(??)   the array used for the two I/O buffers.
C   Common:  INBLK  I   the number of 512 byte blocks in each of the
C                   two buffers. (ie page size).
C            IRRN(1) I   first relative record number (1 relative) in
C                    I/O buffer one.
C            IRRN(2) I   first relative record number in I/O buffer two
C            IMAXS   the maximum number of sectors (blocks) in the
C                    bit map file.
C            IMIDS   I   the first word in SCRTCH used for I/O buffer
C                    two.
C            XMWPF   R   the maximum number of words in the bit map.
C            XWPL    R   the number of words per line in bit map.
C            XWPP    R   the number of words per 'page' in bit map.
C   Outputs: IERR    I   error code passed from ZMI3.
C   Common:  IRRN(1) I   page number  of buffer one updated after read.
C            IRRN(2) I   page number of buffer two updated after read.
C-----------------------------------------------------------------------
      REAL      XWIF, XPAGE
      INTEGER   IBITS(64), SCRTCH(*), IX(1), IY(1), IBUFF, IEND,
     *   IERR, ILUN, IND, IN, INWRDS, IPAGE, ISTART, ISWRD, IWBIT,
     *   IWORD, IWRD, IP, IWIP, NBYTES, IBLKNO(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTPL.INC'
C-----------------------------------------------------------------------
C                                       Loop for number of bits.
      DO 100 IP = 1,IN
C                                       Find word in file starting from
C                                       the first of the file.
         XWIF = XWPL*REAL(IY(IP))  -  REAL((IX(IP)-1)/NBITWD) - 1.0
         XWIF = XMWPF - XWIF
C                                       Find page in file.
         XPAGE = AINT((XWIF-1) / XWPP  +  .0001)
         IPAGE = XPAGE + .0001
C                                       Find word in page.
         IWIP = XWIF - (XPAGE * XWPP)  +  0.0001
C                                       Find word in bit.
         IWBIT = NBITWD - MOD (IX(IP)-1, NBITWD)
C                                       See if RRN is in core.
         IBUFF = 1
         IF (IPAGE.EQ.IRRN(1)) GO TO 40
         IBUFF = 2
         IF (IPAGE.EQ.IRRN(2)) GO TO 40
C                                       Not in core.
C                                       Determine farthest buffer.
         IBUFF = 1
         ISWRD = 1
         INWRDS = INBLK * 256
         IF (ABS (IRRN(2)-IPAGE).GT.IABS (IRRN(1)-IPAGE)) IBUFF = 2
         IF (IBUFF.EQ.2) ISWRD = IMIDS
C                                       See if we need to save
         IF (IRRN(IBUFF).EQ.-9999) GO TO 20
C                                       Save previous data.
            IWRD = ISWRD
            ISTART = INBLK * IRRN(IBUFF) + 2
            IEND = MIN (ISTART + INBLK - 1, IMAXS)
            IF ((ISTART.GT.0) .AND. (ISTART.LE.IEND)) GO TO 10
               WRITE (MSGTXT,1000) IBUFF,IRRN(IBUFF)
               CALL MSGWRT (6)
               IERR = 0
               GO TO 20
 10         NBYTES = (IEND - ISTART + 1) * 512
            IBLKNO(2) = 0
            IBLKNO(1) = (ISTART-1) * (512/NBPS) + 1
            CALL ZMIO ('WRIT', ILUN, IND, IBLKNO, NBYTES, SCRTCH(IWRD),
     *         IBUFF, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (ILUN, IND, IBUFF, IERR)
            IF (IERR.EQ.0) GO TO 20
               WRITE (MSGTXT,1010) IERR
               CALL MSGWRT (8)
               GO TO 999
C                                       Load new page.
 20      IWRD = ISWRD
C                                       Determine start of page.
         ISTART = INBLK * IPAGE  +  2
         IEND = MIN (ISTART + INBLK - 1, IMAXS)
         IF ((ISTART.GT.0) .AND. (ISTART.LE.IEND)) GO TO 25
            WRITE (MSGTXT,1020) IX(IP),IY(IP),IPAGE
            CALL MSGWRT (8)
            GO TO 999
 25      NBYTES = (IEND - ISTART + 1) * 512
         IBLKNO(2) = 0
         IBLKNO(1) = (ISTART-1) * (512/NBPS) + 1
         CALL ZMIO ('READ', ILUN, IND, IBLKNO, NBYTES, SCRTCH(IWRD),
     *      IBUFF, IERR)
         IF (IERR.EQ.0) CALL ZWAIT (ILUN, IND, IBUFF, IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1025) IERR
            CALL MSGWRT (8)
            GO TO 999
 30         CONTINUE
         IRRN(IBUFF) = IPAGE
C                                       Calculate word in buffer.
 40      IWORD = IWIP + (IBUFF-1) * (IMIDS-1)
         IF (IWIP.GT.0) GO TO 45
            WRITE (MSGTXT,1040) IWIP,IX(IP),IY(IP),XWIF
            GO TO 100
C                                       Set bit.
 45      CALL ZGTBIT (NBITWD, SCRTCH(IWORD), IBITS)
         IBITS(IWBIT) = IXBIT
         CALL ZPTBIT (NBITWD, SCRTCH(IWORD), IBITS)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRSBIT: ILL WRITE BLOCKS-BUF# IRRN',2I7)
 1010 FORMAT ('PRSBIT: DISK WRITE ERROR', I4)
 1020 FORMAT ('PRSBIT: ILL READ BLOCKS-X,Y,PAGE',3I7)
 1025 FORMAT ('PRSBIT: DISK READ ERROR', I4)
 1040 FORMAT ('PRSBIT: INDEX=',I7,' BAD AT X,Y,XWIF=',2I7,F12.1)
      END
      SUBROUTINE PRTDRW (LUN, FIND, IVOL, ICENT, ISIZE, ASPMM, NCOPY,
     *   BIGBLK, IERR)
C-----------------------------------------------------------------------
C   This routine will read a graph file and convert the commands into
C   a bit map.  Then the bit map is queued to the printer by calling
C   ZDOPRT.
C   INPUTS:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            IVOL    I   disk volume number of graph file.
C            ICENT   I   if 1 center on page. If -1 write across pages.
C            ISIZE   I   number of words in BIGBLK.
C            ASPMM   R   Requested arcsec / mm (0 => self scale)
C            BIGBLK  I(ISIZE)   buffer for bit map I/O.
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, ICENT, ISIZE, NCOPY, BIGBLK(*), IERR
      REAL      ASPMM
C
      CHARACTER SFILE*48, STRING*132
      REAL      DX, DY, GROFF, GRSCAL, SCALEF, X, WORDS, XYRATO, Y,
     *   NOWPIX, RX1, RX2, RY1, RY2, AX, AY, XPXSEP, YPXSEP, UASPMM,
     *   YASPMM, XYDIFF, DELTA, YMMXMM, YPRDMM, VALUE, TOTPIX
      INTEGER   IOBLK(256), IBITS(256), IXPT(256), IYPT(256), GRYERR,
     *   I3, IANGL, IBLKS, IBMIND, IBMLUN, ICHB, ICHL, ICHR, ICHT, IEND,
     *   IERR2, IERRC, IERRLM, ISTART, ISWRD, IX, IX1, IX2, IY, NXA,
     *   NYA, OPCODE, IBLKNO(2), NBYTES, KOFF, JOFF, IOPOS, IORRN,
     *   ISCVOL, ITYPE, IY1, IY2, NYA0, NPTSX, NPTSY, IGRLO, IGRHI,
     *   NXDPP, NYDPP, NBITS, IPER, NCHAR, INCRRN, NPIX, N, J, L, NPTS,
     *   K, IPER2
      LOGICAL   EXCL, WAIT, DOGRAY, T
      HOLLERITH HOBLK(256)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTPL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IOBLK, HOBLK)
      EQUIVALENCE (WAIT, EXCL)
      DATA IBMLUN, I3  /28, 3/
      DATA EXCL, T /.TRUE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize all values.
C                                       Assume symetric device for now.
      YPRDMM = XPRDMM
      YMMXMM = YPRDMM / XPRDMM
      IERR = 0
      IERRC = 0
      IERRLM = 20
      GRYERR = 0
      IOPOS = 9999
      DOGRAY = .FALSE.
      IYSPAC = 3
      LINWID = 2
      IF (MAXXPR(1).LT.1101) LINWID = 1
C                                       determine where plot starts
      CALL ZFIO ('READ', LUN, FIND, 1, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      IORRN = (IOBLK(10) + 9) / 256 + 1
C                                       next plot command
 50   IF (IOPOS.LE.256) GO TO 60
C                                       Read record.
 55      IORRN = IORRN + 1
         CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         IOPOS = 1
C                                       Transfer based on opcode.
 60   OPCODE = IOBLK(IOPOS)
      IF (OPCODE.LT.0) GO TO 970
      IF (OPCODE.EQ.0) GO TO 55
      IF (OPCODE.LE.8) GO TO (100,200,300,400,500,600,700,800), OPCODE
      IF (OPCODE.NE.32767) GO TO 970
C                                       End of file.
      IBLKNO(2) = 0
      IF (IRRN(1).EQ.-9999) GO TO 70
C                                       Save buffer one.
         ISTART = INBLK * IRRN(1)  +  2
         IEND = MIN(ISTART + INBLK - 1, IMAXS)
         ISWRD = 1
         IBLKNO(1) = (ISTART-1) * (512/NBPS) + 1
         NBYTES = (IEND-ISTART+1) * 512
         CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES,
     *      BIGBLK(ISWRD), 1, IERR)
         IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Save buffer two.
 70   IF (IRRN(2).EQ.-9999) GO TO 80
         ISTART = INBLK * IRRN(2)  +  2
         IEND = MIN(ISTART + INBLK - 1, IMAXS)
         ISWRD = IMIDS
         IBLKNO(1) = (ISTART-1) * (512/NBPS) + 1
         NBYTES = (IEND-ISTART+1) * 512
         CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES,
     *      BIGBLK(ISWRD), 1, IERR)
         IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Queue bit map to printer.
 80   CALL ZCLOSE (IBMLUN, IBMIND, IERR)
      CALL ZDOPRT (ISCVOL, IBMLUN, NCOPY, SFILE, ISIZE, BIGBLK, IERR)
      IF (IERR.EQ.0) GO TO 995
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (6)
         GO TO 995
C-----------------------------------------------------------------------
C                                       Opcode 1, initialize.
 100  CONTINUE
      ITYPE = IOBLK(IOPOS+5)
      IOPOS = IOPOS + 6
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 2, line drawing init.
 200  XYRATO = IOBLK(IOPOS+1) / 100.0
      SCALEF = IOBLK(IOPOS+2)
      IX1    = IOBLK(IOPOS+3)
      IY1    = IOBLK(IOPOS+4)
      IX2    = IOBLK(IOPOS+5)
      IY2    = IOBLK(IOPOS+6)
      RX1    = IOBLK(IOPOS+7)/1000.0
      RY1    = IOBLK(IOPOS+8)/1000.0
      RX2    = IOBLK(IOPOS+9)/1000.0
      RY2    = IOBLK(IOPOS+10)/1000.0
      ICHL   = (IOBLK(IOPOS+11)/10.0) * CSIZPR(1) + 0.5
      ICHB   = (IOBLK(IOPOS+12)/10.0) * (CSIZPR(2) + IYSPAC) + 0.5
      ICHR   = (IOBLK(IOPOS+13)/10.0) * CSIZPR(1) + 0.5
      ICHT   = (IOBLK(IOPOS+14)/10.0) * (CSIZPR(2) + IYSPAC) + 0.5
      IOPOS = IOPOS + 20
C                                       Find no. dots inside border.
      NXA = MAXXPR(1) - ICHL - ICHR - 4
      IF (NXA.GT.0) GO TO 210
         WRITE (MSGTXT,1200) NXA
         CALL MSGWRT (7)
         GO TO 990
C                                        self scale
 210  AX = ABS (REAL(IX2) - REAL(IX1)) * XYRATO
      AY = ABS (REAL(IY2) - REAL(IY1))
      X = ABS (IX2+RX2-IX1-RX1) * XYRATO
      Y = ABS (IY2+RY2-IY1-RY1)
      IF ((X.GT.0) .AND. (Y.GT.0)) GO TO 220
         WRITE (MSGTXT,1210) X, Y
         CALL MSGWRT (7)
         GO TO 990
 220  SCALEX = NXA/SCALEF * AX/X
      SCALEY = SCALEX * AY/AX * YMMXMM
C                                       Arcsec / mm scaling ?
      LOCNUM = 1
      AXTYP(LOCNUM) = 0
      UASPMM = ASPMM
      IF ((ITYPE.NE.2) .AND. (ITYPE.NE.3) .AND. (ITYPE.NE.6)) GO TO 223
         CALL SETLOC (IOBLK(IOPOS-5), T)
         IF (AXTYP(LOCNUM).NE.1) GO TO 223
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
            YPXSEP = XPXSEP
            CALL SCALMM (AX, AY, XPXSEP, YPXSEP, SCALEF, XPRDMM,
     *         YPRDMM, ASPMM, SCALEX, SCALEY)
            GO TO 225
C                                       Scale it down?
 223  IF ((ASPMM.LT.0.02) .OR. (ASPMM.GT.0.98)) GO TO 225
         AXTYP(LOCNUM) = -10
         WRITE (MSGTXT,1223) ASPMM
         CALL MSGWRT (4)
C                                       Center
 225  NXA = SCALEX * SCALEF * X/AX + ICHL + ICHR + .5
      NYA0 = SCALEY * SCALEF * Y/AY + ICHB + ICHT
C                                       Rescale to fit on page.
      IF ((NYA0.LE.MAXXPR(2)) .OR. (ICENT.EQ.-1)) GO TO 230
         NYA0 = MAXXPR(2)
         SCALEY = (NYA0 - ICHT - ICHB - 4) / SCALEF * AY / Y
         SCALEX = SCALEY * AX / AY / YMMXMM
         IF (AXTYP(LOCNUM).EQ.1) CALL SCALMM (AX, AY, XPXSEP, YPXSEP,
     *      SCALEF, XPRDMM, YPRDMM, ASPMM, SCALEX, SCALEY)
         NXA = SCALEX * SCALEF * X/AX + ICHL + ICHR
C                                       Factor it down
 230  IF (AXTYP(LOCNUM).NE.-10) GO TO 240
         SCALEX = SCALEX * ASPMM
         SCALEY = SCALEY * ASPMM * YMMXMM
         NXA = SCALEX * SCALEF * X/AX + ICHL + ICHR + .5
         NYA0 = SCALEY * SCALEF * Y/AY + ICHB + ICHT
 240  IF (IOBLK(IOPOS).NE.3) GO TO 370
C-----------------------------------------------------------------------
C                                       Opcode 3, gray scale init
 300  NPTSX = IOBLK(IOPOS+3)
      NPTSY = IOBLK(IOPOS+4)
      IGRLO = IOBLK(IOPOS+1)
      IGRHI = IOBLK(IOPOS+2)
      IF (IGRHI.LE.IGRLO) GO TO 305
      IF ((NPTSX.LE.0) .OR. (NPTSY.LE.0)) GO TO 305
C                                       Error from GINITG
      IF (NPTSX.LE.MAXXPR(1)-ICHL-ICHR) GO TO 320
 305     WRITE (MSGTXT,1305) NPTSX, NPTSY, IGRLO, IGRHI
         CALL MSGWRT (7)
         GO TO 50
C
 320  IOPOS = IOPOS + 5
      NXDPP = (NXA - ICHL - ICHR) * XYRATO / X + 0.05
      NYDPP = (NYA0 - ICHB - ICHT) / Y + 0.05
C                                       Patch on a bunch of tests to
C                                       correct bugs.
      IF (NXDPP*X/XYRATO.GT.MAXXPR(1)-ICHL-ICHR) NXDPP = NXDPP - 1
      IF (NYDPP*Y.GT.MAXXPR(2)-ICHT-ICHB) NYDPP = NYDPP - 1
      NXDPP = MAX (1, NXDPP)
      NYDPP = MAX (1, NYDPP)
      IF (NYDPP.EQ.NXDPP+1) NYDPP = NXDPP
      IF (NXDPP.EQ.NYDPP+1) NXDPP = NYDPP
C                                       reset scaling
      SCALEX = NXDPP * (AX / XYRATO) / SCALEF
      SCALEY = NYDPP * AY / SCALEF
      NXA = ICHL + ICHB + NXDPP * X / XYRATO
      NYA0 = ICHB + ICHT + NYDPP * Y
      IF (AXTYP(LOCNUM).EQ.1) ASPMM = ABS (AX*XPXSEP*XPRDMM) /
     *   (SCALEX*SCALEF)
C                                       Initialize random no. generator
      VALUE = 0.0
      NBITS = -1
      CALL GRYRAN (VALUE, NBITS, IBITS)
      NBITS = NXDPP
C                                       Array not big enough.
      IF (NBITS.LE.256) GO TO 350
         WRITE (MSGTXT,1330) NXDPP, NYDPP
         CALL MSGWRT (7)
         GO TO 990
C
 350  DOGRAY = .TRUE.
      KOFF = 1 + NXDPP / 2
      JOFF = 1 + NYDPP / 2
      GROFF = IGRLO
      GRSCAL = 1.0/(REAL(IGRHI)-REAL(IGRLO))
C                                       Init variables for progress
C                                       report.
      TOTPIX = REAL(NPTSX) * REAL(NPTSY)
      NOWPIX = 0.0
      IPER = 0
      WRITE (MSGTXT,1350)
      CALL MSGWRT (1)
C                                       set last of parms
 370  NYA = NYA0 + MAX (0, MAXXPR(2) - NYA0) / 2  + 2
      IX0 = ICHL + MAX (0, MAXXPR(1) - NXA) / 2  +  1 -
     *   RX1 * XYRATO * SCALEX * SCALEF / AX
      IY0 = ICHB +  1 - RY1 * SCALEY * SCALEF / AY
C                                       Print arc sec per mm info.
      IF (AXTYP(LOCNUM).NE.1) GO TO 380
         YASPMM = ABS (AY * AXINC(2,LOCNUM) * 3600.0 * YPRDMM)
     *      / (SCALEY * SCALEF)
         XYDIFF = ABS (ASPMM - YASPMM)
         DELTA = .05 * ASPMM
C                                       X and Y ASPMM about the same.
         IF (XYDIFF.GT.DELTA) GO TO 375
            WRITE (MSGTXT,1370) ASPMM
            CALL MSGWRT (4)
            GO TO 379
C                                       Using different X and Y ASPMM
 375     CONTINUE
            WRITE (MSGTXT,1375) ASPMM
            CALL MSGWRT (4)
            WRITE (MSGTXT,1376) YASPMM
            CALL MSGWRT (4)
C                                       changed user's ASPMM
 379     IF ((UASPMM.LE.0.0) .OR. (UASPMM.GE.ASPMM)) GO TO 380
            WRITE (MSGTXT,1379)
            CALL MSGWRT (4)
C                                       Open bit map file.
 380  CALL PRBMCR (IVOL, IBMLUN, IBMIND, MAXXPR(1), NYA,
     *   SFILE, ISCVOL, IBLKS, WORDS, BIGBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Set common variables.
      IMAXS = IBLKS
      XMWPF = WORDS
      XWPL = (MAXXPR(1) - 1) / NBITWD + 1
      IMIDS = ISIZE / 2  +  1
      INBLK = (ISIZE / 2) / 256
      XWPP = INBLK * 256
      MAXXPR(2) = NYA
      IRRN(1) = -9999
      IRRN(2) = -9999
C                                       Initialize bit map file.
      CALL PRFINT (NYA, IBMLUN, IBMIND, IOPOS, IOBLK, ISIZE, BIGBLK,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (MSGTXT,1380)
      CALL MSGWRT (1)
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 4, position cursor.
 400  IX = IOBLK(IOPOS+1)
      IY = IOBLK(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL PRTVEC (IX, IY, 1, IBMLUN, IBMIND, BIGBLK, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 5, draw vector.
 500  IX = IOBLK(IOPOS+1)
      IY = IOBLK(IOPOS+2)
      IOPOS = IOPOS+3
      CALL PRTVEC (IX, IY, 2, IBMLUN, IBMIND, BIGBLK, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 6, write characters.
 600  NCHAR = IOBLK(IOPOS+1)
      IANGL = IOBLK(IOPOS+2)
      DX    = IOBLK(IOPOS+3)/100.0
      DY    = IOBLK(IOPOS+4)/100.0
      IX = IXL + DX * CSIZPR(1)
      IY = IYL + DY * (CSIZPR(2) + IYSPAC)
      CALL PRTVEC (IX, IY, I3, IBMLUN, IBMIND, BIGBLK, IERR)
      IF (IERR.EQ.0) THEN
         CALL H2CHR (NCHAR, 1, HOBLK(IOPOS+5), STRING)
         CALL PRCHAR (NCHAR, IANGL, STRING, IBMLUN, IBMIND, BIGBLK,
     *      IERR)
         END IF
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 960
      IOPOS = IOPOS + 5 + ((NCHAR+3)/4)
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 7, write grey scale.
C                                       ignore gray scale
 700  IF (DOGRAY) GO TO 710
         GRYERR = GRYERR + 1
C                                       Update IO position. Read
C                                       proper RRN if necessary.
         IOPOS = IOPOS + 3 + IOBLK(IOPOS+1)
         IF (IOPOS.LE.256) GO TO 60
C                                       Update position & buffer.
            INCRRN = (IOPOS-1)/256
            IORRN = IORRN + INCRRN
            IOPOS = IOPOS - 256*INCRRN
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
            IF (IERR.EQ.0) GO TO 60
            GO TO 980
C                                       Get gray values from record(s).
 710  NPIX = IOBLK(IOPOS+1)
      IANGL = IOBLK(IOPOS+2)
      IOPOS = IOPOS + 3
      IF (NPIX.LE.0) GO TO 50
C                                       Set bits for each pixel.
      DO 790 N = 1,NPIX
C                                       Need another buffer of G values
         IF (IOPOS.LE.256) GO TO 720
            IORRN = IORRN + 1
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            IOPOS = 1
C
 720     VALUE = (IOBLK(IOPOS) - GROFF) * GRSCAL
C                                       Loop for each bit in pixel.
         IERR = 0
         DO 750 J = 1,NYDPP
            CALL GRYRAN (VALUE, NBITS, IBITS)
            L = 0
            NPTS = 0
            DO 730 K = 1,NXDPP
               L = L + 1
               IF (IBITS(L).EQ.0) GO TO 730
                  NPTS = NPTS + 1
                  IXPT(NPTS) = IXL + K - KOFF
                  IYPT(NPTS) = IYL + J - JOFF
 730           CONTINUE
            IF (NPTS.NE.0) CALL PRSBIT (IXPT, IYPT, NPTS, IBMLUN,
     *         IBMIND, BIGBLK, IERR)
            IF (IERR.NE.0) GO TO 980
 750        CONTINUE
C
         IF (IANGL.EQ.0) IXL = IXL + NXDPP
         IF (IANGL.NE.0) IYL = IYL + NYDPP
         IOPOS = IOPOS + 1
 790     CONTINUE
      NOWPIX = NOWPIX + NPIX
      IPER2 = INT(5.0 * NOWPIX / TOTPIX)
      IF (IPER2.EQ.IPER) GO TO 50
C                                       Write progress report.
      IPER = 20*IPER2
      WRITE (MSGTXT,1790) IPER
      IPER = IPER2
      CALL MSGWRT (1)
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 8 from GMCAT, ignore.
 800  IOPOS = IOPOS + 2 + IOBLK(IOPOS+1)
      GO TO 50
C-----------------------------------------------------------------------
C                                       Write error.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      GO TO 990
C                                       Invalid opcode.
 970  WRITE (MSGTXT,1970) OPCODE, IOPOS, IORRN
      CALL MSGWRT (8)
      GO TO 990
C                                       Disk error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close files.
 990  CALL ZCLOSE (IBMLUN, IBMIND, IERR2)
      IF (GRYERR.EQ.0) GO TO 995
         WRITE (MSGTXT,1990) GRYERR
         CALL MSGWRT (3)
C
 995  IF (IERRC.LE.0) GO TO 999
         WRITE (MSGTXT,1995) IERRC
         CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('PRTDRW: BIT MAP CREATED BUT COULD NOT WRITE TO PRINTER',
     *   I7)
 1200 FORMAT ('PRTDRW: ERR FROM GINITL ARGS. NXA=',I6)
 1210 FORMAT ('PRTDRW: ERR FROM GINITL ARGS. X,Y=',2E10.3)
 1223 FORMAT ('Plot reduced in size by factor',F6.3)
 1305 FORMAT ('PRTDRW: ERROR FROM GINITG',4I6)
 1330 FORMAT ('PRTDRW: ARRAY FOR BITS TOO SMALL. BITS=',I5,'X',I5)
 1350 FORMAT ('Random number generator initialized')
 1370 FORMAT ('Using',1PE12.5,' arc sec per mm')
 1375 FORMAT ('Using',1PE12.5,' arc sec per mm for x axis')
 1376 FORMAT ('Using',1PE12.5,' arc sec per mm for y axis')
 1379 FORMAT ('Overriding user aspmm in order to fit plot on page')
 1380 FORMAT ('Bit map file initialized')
 1790 FORMAT (I3,' %  of grey scale pixels plotted')
 1960 FORMAT ('PRTDRW: WRITE ERROR LIMIT EXCEEDED')
 1970 FORMAT ('PRTDRW: INVALID OPCODE',I6,' AT POS,REC',2I6)
 1980 FORMAT ('PRTDRW: DISK IO RELATED ERROR',I3)
 1990 FORMAT ('PRTDRW: ',I5,' grey scale opcodes ignored')
 1995 FORMAT (I8,' vectors truncated at edges')
      END
      SUBROUTINE PRTVEC (IX, IY, IN, ILUN, IND, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   PRTVEC scales coordinates, sets bits to represent lines in
C   the bit map and saves latest coordinate values.
C   Inputs: IX      I         X position
C           IY      I         Y position
C           IN      I         1 => scale IX,IY but no vector
C                             2 => scale IX,IY write vector
C                             3 => no scaling, no vector
C                             4 => no scaling, write vector
C           ILUN    I         bit map file logical unit number.
C           IND     I         pointer to bit map info in FTAB.
C   Output: SCRTCH  I(??)    scratch buffer
C           IERR    I         error code of ZMI3
C-----------------------------------------------------------------------
      INTEGER   IX, IY, IN, ILUN, IND, SCRTCH(*), IERR
C
      REAL      T, XI, XMAX, ALPHA, BETA
      INTEGER   IXT(2), IYT(2), I, IMAX, IXN, IYN, X1, X2, Y1, Y2,
     *   IABSX, IABSY
      LOGICAL   ISERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTPL.INC'
C-----------------------------------------------------------------------
C                                        scale
      IERR = 0
      ISERR = .FALSE.
      IXN = IX
      IYN = IY
      IF (IN.GT.2) GO TO 10
         IXN = IXN * SCALEX  +  IX0 + 0.5
         IYN = IYN * SCALEY  +  IY0 + 0.5
C                                        test
 10   IF ((IN.NE.2) .AND. (IN.NE.4)) GO TO 50
         X1 = MAX (1, MIN (MAXXPR(1), IXL))
         X2 = MAX (1, MIN (MAXXPR(1), IXN))
         Y1 = MAX (1, MIN (MAXXPR(2), IYL))
         Y2 = MAX (1, MIN (MAXXPR(2), IYN))
         IF ((X1.EQ.IXL) .AND. (Y1.EQ.IYL)) GO TO 15
            ALPHA = 1.0
            IF (IXL.NE.IXN) ALPHA = REAL(X1-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYL.NE.IYN) BETA = REAL(Y1-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X1 = IXL + ALPHA * (IXN-IXL)
            Y1 = IYL + ALPHA * (IYN-IYL)
            ISERR = .TRUE.
 15      IF ((X2.EQ.IXN) .AND. (Y2.EQ.IYN)) GO TO 20
            ALPHA = 1.0
            IF (IXL.NE.IXN) ALPHA = REAL(X2-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYL.NE.IYN) BETA = REAL(Y2-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X2 = IXL + ALPHA * (IXN-IXL)
            Y2 = IYL + ALPHA * (IYN-IYL)
            ISERR = .TRUE.
 20      CONTINUE
C                                       Check for entire line off plot.
         IF ((X1.LT.1) .OR. (X1.GT.MAXXPR(1)) .OR.
     *      (Y1.LT.1) .OR. (Y1.GT.MAXXPR(2)) .OR.
     *      (X2.LT.1) .OR. (X2.GT.MAXXPR(1)) .OR.
     *      (Y2.LT.1) .OR. (Y2.GT.MAXXPR(2)) ) GO TO 50
C                                       Proceed to draw vector
            XI = 0.0
            IABSX = ABS (X1 - X2)
            IABSY = ABS (Y1 - Y2)
            IMAX = MAX (IABSX, IABSY)
            XMAX = IMAX
            IF (IMAX.EQ.0) XMAX = 1.0
C                                       Loop and set bits between end
C                                       points.
            IMAX = IMAX + 1
            DO 35 I = 1,IMAX
               T = XI / XMAX
C                                       Double wide lines.
C                                       horizontal widening
               IF (IABSX.GT.IABSY) GO TO 25
                  IXT(1) = REAL(X1) + REAL(X2 - X1) * T +
     *               0.5 * (2-LINWID)
                  IXT(2) = IXT(1) + 1
                  IYT(1) = REAL(Y1) + REAL(Y2 - Y1) * T + .5
                  IYT(2) = IYT(1)
                  GO TO 30
C                                       vertical widening
 25            CONTINUE
                  IYT(1) = REAL(Y1) + REAL(Y2 - Y1) * T +
     *               0.5 * (2-LINWID)
                  IYT(2) = IYT(1) + 1
                  IXT(1) = REAL(X1) + REAL(X2 - X1) * T + .5
                  IXT(2) = IXT(1)
 30            XI = XI + 1.0
               CALL PRSBIT (IXT, IYT, LINWID, ILUN, IND, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 50
 35            CONTINUE
C                                        set last position
 50   IXL = IXN
      IYL = IYN
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE PRFINT (NYA, IBMLUN, IBMIND, IOPOS, IOBLK, ISIZE,
     *   ISCBLK, IERR)
C-----------------------------------------------------------------------
C   PRFINT will find the first 'long' vertical line (.25 of maximum
C   vertical) and a second vertical line with corresponding Y end
C   points (if the commands for this line is in the first block of
C   graphics commands).  The bits representing these lines are set
C   during initialization of the bit map file.
C   INPUTS:   NYA     I        The number of print lines in the bit map
C                              bit map file.
C             IBMLUN  I        The bit map file LUN.
C             IBMIND  I        The bit map file FTAB position.
C             IOPOS   I        The position in the block containing the
C                              graphics commands to start looking.
C             IOBLK   I(256)   The buffer containing the graphics
C                              commands.
C             ISIZE   I        Size of ISCBLK in words
C             ISCBLK  I(***)   Scratch buffer used for I/O.
C   COMMON:   IMAXS   I        Maximum number of sectors in the file.
C   OUTPUT:   IERR    I        Error code. 0=ok; 1=I/O error.
C-----------------------------------------------------------------------
      INTEGER   IBITS(64), IOBLK(256), ISCBLK(*), IWINC(4), ISIZE,
     *   IWORD(4), IXS(3), IFCHAR, IACHAR, IBLKNO(2), NBYTES,
     *   I2POS, IOPOS, IN, IBLK, IEBLK, ICODE, IX, IY,
     *   IDUM, IERR, IX1, IY1, NYA, IYTOP, IYBOT, NCHAR, IANGLE,
     *   IWOUT, ITEMP, IWPL, ITX, ISBLK, INW, ISTART, I, IBIT, INDEX,
     *   IBMLUN, IBMIND, IWRD, J
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTPL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IFCHAR = 1 - IXBIT
      CALL FILL (NBITWD, IFCHAR, IBITS)
      CALL ZPTBIT (NBITWD, IACHAR, IBITS)
      I2POS = IOPOS
      IN = 0
      IBLK = 0
      IEBLK = 1
C                                       look for vertical lines
 100  IF (I2POS.GT.256) GO TO 800
      ICODE = IOBLK(I2POS)
      IF ((ICODE.LT.1) .OR. (ICODE.GT.8)) GO TO 800
      GO TO (800, 800, 800, 400, 500, 600, 800, 200) , ICODE
C                                       Catalog info command
 200  I2POS = I2POS + 2 + IOBLK(I2POS+1)
      GO TO 100
C                                       position command
 400  IX = IOBLK(I2POS+1)
      IY = IOBLK(I2POS+2)
      I2POS = I2POS + 3
      CALL PRTVEC (IX, IY, 1, IDUM, IDUM, ISCBLK, IERR)
      GO TO 100
C                                       vector command
 500  IX = IOBLK(I2POS+1)
      IY = IOBLK(I2POS+2)
      I2POS = I2POS + 3
      IX1 = IXL
      IY1 = IYL
      CALL PRTVEC (IX, IY, 1, IDUM, IDUM, ISCBLK, IERR)
C                                       see if line vert & long
      IF (IX1.NE.IXL) GO TO 100
      IF (ABS(IYL-IY1).LT..25*NYA) GO TO 100
C                                       See if 1st line.
      IF (IN.EQ.0) GO TO 550
C                                       Not first line.
         IF (MAX (IY1, IYL).NE.IYTOP) GO TO 100
         IF (MIN (IY1, IYL).NE.IYBOT) GO TO 100
C                                       we found long vert line
 550  IN = IN + 1
C                                       set code to position
      IOBLK(I2POS-3) = 4
      IXS(IN) = IX1
      IYTOP = MAX(IY1, IYL)
      IYBOT = MIN(IY1, IYL)
      IF (IN.GE.2) GO TO 800
      GO TO 100
C                                       character string command
 600  NCHAR = IOBLK(I2POS+1)
      IANGLE = IOBLK(I2POS+2)
      I2POS = I2POS + 5 + ((NCHAR+3)/4)
      IF (I2POS.GT.256)  GO TO 800
C                                       horizontal string
      IF (IANGLE.EQ.1) GO TO 650
         IXL = IXL + NCHAR * CSIZPR(1)
         GO TO 100
C                                       vertical string
 650  IXL = IXL + CSIZPR(1)
      IYL = IYL - NCHAR * CSIZPR(2)
      GO TO 100
C                                       Init & draw vertical lines
C                                       Put no. of lines in block 1
 800  CALL FILL (ISIZE, IACHAR, ISCBLK)
      ISCBLK(1) = NYA
      NBYTES = 2 * ISIZE
      IBLKNO(2) = 0
      IBLKNO(1) = 1
      IWOUT = 256
      IEBLK = 1
      IF (IN.EQ.0) GO TO 885
      IF (IN.EQ.1) GO TO 830
C                                       Sort X values.
      IF (IXS(1).LT.IXS(2)) GO TO 830
         ITEMP = IXS(1)
         IXS(1) = IXS(2)
         IXS(2) = ITEMP
C                                       Calculate start-stop blocks
 830  IWPL = (MAXXPR(1)-1) / NBITWD + 1
      ITX = (IXS(1) - 1) / NBITWD
      ISBLK = (REAL(NYA - IYTOP) * REAL(IWPL) + REAL(ITX))
     *   / 256.0  +  2.001
      IEBLK = ISBLK + REAL(IYTOP - IYBOT + 1) * REAL(IWPL)
     *   / 256.0 + .001
C                                       calculate words, increments
      INW = 0
      IXS(IN+1) = IWPL * NBITWD + IXS(1)
      ISTART = REAL(IWPL) * REAL(NYA-IYTOP) + REAL(ITX) -
     *   REAL(ISBLK-2) * 256.0  +  1.001
C                                       Loop for number of vert lines.
      DO 850 I = 1,IN
         CALL FILL (NBITWD, IFCHAR, IBITS)
         INW = INW + 1
C                                       Calculate inc to this word.
         IWINC(INW) = (IXS(I+1)-1)/NBITWD  - (IXS(I)-1)/NBITWD
C                                       Calculate and set bit for word.
         IBIT = NBITWD - MOD(IXS(I)-1, NBITWD)
         IBITS(IBIT) = IXBIT
C                                       Set 2nd bit for wide line.
         IBIT = IBIT - 1
         IF (IBIT.GT.0) GO TO 840
C                                       2 words required.
            CALL ZPTBIT (NBITWD, IWORD(INW), IBITS)
            INW = INW + 1
            IWINC(INW) = IWINC(INW-1) - 1
            IWINC(INW-1) = 1
            IBITS(1) = IFCHAR
            IBIT = NBITWD
 840     IF (LINWID.GT.1) IBITS(IBIT) = IXBIT
         CALL ZPTBIT (NBITWD, IWORD(INW), IBITS)
 850     CONTINUE
C                                       loop up to start of line
      INDEX = ISBLK - 1
      IF (INDEX.LT.2) GO TO 865
         DO 860 IBLK = 2,INDEX
            IWOUT = IWOUT + 256
            IF (IWOUT.LT.ISIZE) GO TO 860
               CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES,
     *            ISCBLK, 1, IERR)
               IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
               IF (IERR.NE.0) GO TO 980
                  IWOUT = 0
                  IBLKNO(1) = IBLKNO(1) + NBYTES/NBPS
                  ISCBLK(1) = IACHAR
 860        CONTINUE
C                                       write vertical lines
 865  IWRD = ISTART
      IBLK = ISBLK
C                                       Loop for all lines.
      DO 880 I = IYBOT,IYTOP
C                                       Loop for words to set per line.
         DO 875 J = 1,INW
            ISCBLK(IWRD+IWOUT) = IWORD(J)
            IWRD = IWRD + IWINC(J)
            IF (IWRD.LE.256) GO TO 875
               IWRD = IWRD - 256
               IWOUT = IWOUT + 256
               IBLK = IBLK + 1
               IF (IWOUT.LT.ISIZE) GO TO 875
                  CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES,
     *               ISCBLK, 1, IERR)
                  IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
                  IF (IERR.NE.0) GO TO 980
                     IWOUT = 0
                     IBLKNO(1) = IBLKNO(1) + NBYTES/NBPS
                     CALL FILL (ISIZE, IACHAR, ISCBLK)
 875        CONTINUE
 880     CONTINUE
C                                       finish rest of file
 885  INDEX = IEBLK + 1
      IF (INDEX.GT.IMAXS) GO TO 900
         DO 890 IBLK = INDEX, IMAXS
            IWOUT = IWOUT + 256
            IF (IWOUT.LT.ISIZE) GO TO 890
               CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES,
     *            ISCBLK, 1, IERR)
               IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
               IF (IERR.NE.0) GO TO 980
                  IWOUT = 0
                  IBLKNO(1) = IBLKNO(1) + NBYTES/NBPS
                  CALL FILL (ISIZE, IACHAR, ISCBLK)
 890        CONTINUE
 900  IF (IWOUT.LE.0) GO TO 999
         NBYTES = 2 * IWOUT
         CALL ZMIO ('WRIT', IBMLUN, IBMIND, IBLKNO, NBYTES, ISCBLK, 1,
     *      IERR)
         IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, 1, IERR)
         IF (IERR.EQ.0) GO TO 999
C                                       I/O error.
 980  WRITE (MSGTXT,1980) IERR
      IERR = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('I/O ERROR FROM ZMIO WHILE INITIALIZING BIT MAP. IER=',I5)
      END
