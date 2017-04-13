LOCAL INCLUDE 'FIT2A.INC'
C                                      Local include for FIT2A
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IBLNK
      INTEGER   CATBLK(256), FDVEC(50), TBIND, NBPIX,
     *   TAPEBP, TABLES, INBUFF(MABFSS), TAPBUF(29184)
      LOGICAL   ISBLNK, FUCKUP, STDEXT
      REAL      CATR(256)
      HOLLERITH CATH(256), XINFIL(12), XOFILE(12)
      CHARACTER INFILE*48, OFILE*48, HDRBUF*2880
      DOUBLE PRECISION CATD(128), POS11(2), SCALE, OFFSET, ISCALE,
     *   IZERO
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, TAPBUF,
     *   INBUFF, IBLNK, ISBLNK, FUCKUP, STDEXT, FDVEC, TBIND, NBPIX,
     *   TAPEBP, TABLES
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XINFIL, XOFILE
      COMMON /CHRCOM/ HDRBUF, INFILE, OFILE
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C                                                          End FIT2A
LOCAL END
      PROGRAM FIT2A
C-----------------------------------------------------------------------
C! Reads image from FITS file and records it to the output ascii file
C# Tape Map-util EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 2003-2004, 2010, 2015
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
C   FIT2A is the task which reads the input fits file and records
C   it to the output ascii file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE       Input fits file name.
C      OUTFILE        OFILE        Output asci file name.
C-----------------------------------------------------------------------
      INTEGER   IRET, HLUN, HBUFF(256), ISCR(256), NPARM
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA HLUN, NPARM /27, 24/
C-----------------------------------------------------------------------
      CALL MLINI (NPARM, ISCR, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       the main routune to convert
C                                       FITS format file to ASCII
C                                       format file
      CALL FITASC (ISCR, IRET)
      IF (IRET.NE.0) GO TO 995

C                                       Clean up
 995  CALL DIE (IRET, HBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
      END
C
      SUBROUTINE FITASC (ISCR, IRET)
C-----------------------------------------------------------------------
C     FITASC reads the fits file and records it to ascii file
      INTEGER  IRET
      INTEGER  IOP, FITS, ISCR(256), IERR
      LOGICAL  NODATA
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Convert to internal format.
C
      CALL MLTAPE (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       See what kind of file.
      CALL TPIOHD (FDVEC, 128, FITS, TBIND, TAPBUF, ISCR, IRET)
      IF (IRET.NE.0) GO TO 950
      IF (FITS.EQ.1) THEN
         IOP = 1
         CALL FITHDR (IOP, NODATA, IRET)
C                                       Get the data and store in file
         CALL FITDAT (IRET)
         IF (IRET.NE.0) GO TO 950
      ELSE
         WRITE (MSGTXT,1000)
         IF (FITS.EQ.-1) WRITE (MSGTXT,1001)
         IF ((FITS.EQ.-1) .AND. (FDVEC(42).EQ.2560))
     *      WRITE (MSGTXT,1002)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 950
         END IF
C                                       Close output
 950  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPE IS UV-EXPORT FORMAT: USE UVLOD')
 1001 FORMAT ('TAPE FILE IS NON-STANDARD FITS WHICH IS NOT SUPPORTED')
 1002 FORMAT ('TAPE IS LIKELY TO BE ''RPFITS'' FORMAT: TRY ATLOD')
      END
C
      SUBROUTINE MLINI (NPARM, ISCR, IRET)
C-----------------------------------------------------------------------
C   MLINI initializes FIT2A
C   Outputs:
C      KVOL   I     Output disk #
C      SEQ    I     Output sequence number
C      ISCR   I(256)   Scratch buffer.
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   ISCR(256), IRET, I1
      INTEGER   NPARM, IERR
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'FIT2A '/
C-----------------------------------------------------------------------
C                                       Initialize disk character.
      I1 = 1
      CALL ZDCHIN (.TRUE.)
      CALL HIINIT (3)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Initialize for AIPS
      IRET = 0
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XINFIL, ISCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Check if disk file output
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
C
      IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
C                                       Init FDVEC
      CALL FILL (50, 0, FDVEC)
      GO TO 999
C
 990  IRET = 16
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN GTPARM.  IER=',I7)
      END
C
      SUBROUTINE FITDAT (IER)
C-----------------------------------------------------------------------
C   FITDAT reads the input data file and scales the data to disk.
C   Inputs:
C      KVOL  I     desired map disk
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
C    new cards for read fits file
      INTEGER  NBAD, IX, IY, IND
C-----------
C                                       array size for mask
      INTEGER MXMASK
      PARAMETER (MXMASK = 2000*2000)
      REAL  XBAD(MXMASK), YBAD(MXMASK)
C-----------
      INTEGER   IER
C
      CHARACTER CDECLS(5)*4, CHTM12*12
      INTEGER   BLKS, IERR, NBKOF1, IOFF, NX, NY, IDEPTH(5),
     *   I, IBL, ITEMP, NXY, I3, I3B, I4, I4B, I5, I5B,
     *   I6, I6B, I7, I7B, NTAPVL, J, NDECLS, III, L0, L1,
     *   L2, NXX, OUTIND
      INCLUDE 'FIT2A.INC'
      REAL      BUFF(MABFSS), MMAX, MMIN, INBUFR(MABFSS)
      REAL     RND
      DOUBLE PRECISION    BSC, BZE, DPBUFR(MABFSS/2), DTEMP
      LOGICAL  FA
      INTEGER  LUNPR, PFIND, NCH, ITRIM
      CHARACTER LINE*80
      LOGICAL   T, BACK, WASBLK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUFR(1), INBUFF(1), DPBUFR(1))
      DATA T /.TRUE./
      DATA NDECLS, CDECLS /5, 'DEC ', 'DEC-', 'MM  ', 'GLAT', 'ELAT'/
C-----------------------------------------------------------------------
C                                       open the output file
      IF (OFILE(1:1).NE.' ') THEN
         LUNPR = 10
         FA = .FALSE.
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, FA, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1100)
            GO TO 980
            END IF
         END IF
C!!!!!!!!!!!do not gorget to clean the following cards!!!!!!!!
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
C                                       Initialize
      IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
C                                       second axis backwards?
      BACK = .FALSE.
      IF (CATR(KRCIC+1).GE.0.0) GO TO 10
         J = KHCTP + 2
         DO 6 I = 1,NDECLS
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            BACK = CHTM12(1:4) .EQ. CDECLS(I)(1:4)
            IF (BACK) GO TO 8
  6         CONTINUE
         GO TO 10
  8      CONTINUE
            CATR(KRCIC+1) = -CATR(KRCIC+1)
            CATR(KRCRP+1) = CATBLK(KINAX+1) + 1 - CATR(KRCRP+1)
C                                       Set window parms
 10   I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Initialize tape
      NBPIX = TAPEBP
      BLKS = (ABS(NBPIX) / 8)
      NTAPVL = 2880 / BLKS
      IOFF = NTAPVL
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
Cnew
         NBAD = 0
C                                       Begin read/write loop
         DO 195 I = 1,NY
            NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-64) CALL ZR64RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.EQ.8) .OR. (NBPIX.EQ.16) .OR. (NBPIX.EQ.32))
     *            THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           BUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           BUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, BUFF(L1))
                           MMAX = MAX (MMAX, BUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        BUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 115                    CONTINUE
                     END IF
                  GO TO 190
C                                       IEEE 64-bit in
               ELSE IF (NBPIX.EQ.-64) THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 150 III = 1,NXX
                     L1 = L2 + III
                     DTEMP = DPBUFR(L0+III)
                     IF (DTEMP.EQ.DBLANK) THEN
                        WASBLK = .TRUE.
                        BUFF(L1) = FBLANK
                     ELSE
                        BUFF(L1) = BSC * DTEMP + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 150                 CONTINUE
C                                       IEEE 32-bit in
               ELSE
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     BUFF(L1) = INBUFR(L0+III)
                     IF (BUFF(L1).EQ.FBLANK) THEN
                        WASBLK = .TRUE.
                     ELSE
                        BUFF(L1) = BSC * BUFF(L1) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 160                 CONTINUE
                  END IF
C                                       Up the counters
 190           IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
C---------------------------------------------------------
C            new cards to read the fits file for CONFI
            IY = I
            DO 170 IX = 1, NX
               IND = BUFF (IX + OUTIND - 1)
               RND = BUFF (IX + OUTIND - 1)
C                                       record to the output file
               IF (OFILE(1:1).NE.' ') THEN
                  WRITE (LINE, 1070) IX, IY, RND
Ctemporaly consider that IND=0,1
C                  WRITE (LINE, 1070) IX, IY, IND
                  NCH = ITRIM(LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
                  END IF
C                                       select restricted points
C               IF (IND .GT. 0.9) THEN
               IF (IND .LT. 0.1) THEN
                  NBAD = NBAD + 1
                  IF (NBAD .GT. MXMASK) THEN
                     IERR = 5
                     WRITE (MSGTXT,1040) MXMASK
                     GO TO 980
                     END IF
                  XBAD(NBAD) = IX
                  YBAD(NBAD) = IY
                  END IF
  170          CONTINUE
C-----------------------------------------------------------
 195         CONTINUE
C
 200     CONTINUE
C                                       close files
C                                       close the output file
      IF (OFILE(1:1).NE.' ') CALL ZTXCLS (LUNPR, PFIND, IERR)
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1980)
         CALL MSGWRT(8)
         END IF
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Can not open the given output file')
 1040 FORMAT ('!Number of restricted pixels at the mask',
     *   ' exceeds limit', I8)
 1070 FORMAT (I4, I5, 1X, E12.6)
 1970 FORMAT ('FITDAT: COULD NOT READ INPUT.  IER=',I4)
 1980 FORMAT ('FITDAT: - MAYBE PREMATURE END OF FILE?  CHECK FILE SIZE')
      END
C
      SUBROUTINE IMREQC (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, EXTEND*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
      DATA EXTEND /'EXTEND  '/
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      I = NCT + NKT
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
      IF (NAXIS.LT.1) GO TO 40
C                                       Check NAXISm
         DO 30 I = 1,NAXIS
            ICARD = ICARD + 1
            IKEYWD = IKEYWD + 1
            NPNT = 1
            CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *         KL, SYMBOL, ITABNO, ISHIST, END, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            CATBLK(IAX) = X + .01
            IAX = IAX + 1
 30         CONTINUE
C                                       Look for EXTEND = T card.
 40   CONTINUE
      IF (CATBLK(KINAX).EQ.0) GO TO 930
      ICARD = ICARD + 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, EXTEND, FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, ITYP)
      IF (END.OR.ISHIST.OR.(ITYP.NE.0)) GO TO 50
          CALL GETLG (KL, 80, NPNT, ITYP)
          IF (ITYP.NE.1) GO TO 50
          STDEXT = .TRUE.
          GO TO 999
 50   CONTINUE
C                                       No extensions
         ICARD = ICARD - 1
         STDEXT = .FALSE.
      GO TO 999
C                                       Probably a UV tape.
 930  WRITE (MSGTXT,1930)
      CALL MSGWRT (8)
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'IMREQC: VALUE ERROR PARSING ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('THIS IS PROBABLY A UV DATA FILE THAT MUST BE READ WITH',
     *   ' UVLOD')
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
C
      SUBROUTINE MLTAPE (IERR)
C-----------------------------------------------------------------------
C   MLTAPE sets up for TAPIO and opens the input for FIT2A.
C   Outputs: IERR        I    Error return
C                             0--> okay,  1--> error
C   Uses and build special common /MLTAPE/
C-----------------------------------------------------------------------
      INTEGER  IERR
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
C                                       Disk output.
      CALL CHR2H (48, INFILE, 1, FDVEC(7))
      WRITE (MSGTXT,1000) INFILE
      CALL MSGWRT (2)
      FDVEC(1) = 25
      FDVEC(5) = 1
C                                       Open tape
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 980
         END IF
      GO TO 999
C                                       Error returns
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reading from disk file: ',A)
 1025 FORMAT ('MLTAPE: COULD NOT OPEN TAPE.  IER=',I7)
      END
C
      SUBROUTINE FITHDR (IOP, NODATA, IERR)
C-----------------------------------------------------------------------
C   FITHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     IOP    I     Operation code 1=> read tape, 2=>just create dummy
C                  file
C   Output:
C     NODATA L     True if tape contains no data section, else false.
C     IERR   I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      REAL      PIX11(2)
      INTEGER   IOP, ICARD, IE, IERR, IREC, I,
     *   IN, IS, IAX, IDEPTH(5), ICEND
      LOGICAL   END, F, T, ISHIST, NODATA
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      FUCKUP = .FALSE.
      CALL CATCLR (CATBLK)
C                                       See if tape read requested
      IF (IOP.EQ.2) GO TO 15
C                                       Initialize header values.
         CALL CATINI (CATBLK)
         SCALE = 1.0D0
         OFFSET = 0.0D0
         ISCALE = 1.0D0
         IZERO = 0.0D0
C                                       Record 1 already read
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       Decode required cards.
         CALL IMREQC (HDRBUF, ICEND, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (CATBLK(KIDIM).NE.0) GO TO 5
C                                       No data, must have tables.
            IF (.NOT.STDEXT) GO TO 980
C                                       Make a 2x2 map.
 15   NODATA = .TRUE.
      ISBLNK = .TRUE.
      CATBLK(KIDIM) = 2
            CATBLK(KINAX) = 2
            CATBLK(KINAX+1) = 2
C                                       More defaults.
 5    DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = CATBLK(KINAX+I-1) / 2
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'FIT2A       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
C      ISLOT = CNO
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1000)
         GO TO 990
 20   NCFILE = 1
C      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
C      FVOL(NCFILE) = KVOL
C                                       See if need to parse rest of
C                                       header.
      IF (IOP.NE.2) THEN
C
         ICARD = ICEND + 1
C                                       Loop until END card found.
         DO 90 IREC = 1,1000000
C                                       Read next record.
            IF (ICARD.LE.36) GO TO 80
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
               ICARD = 1
C                                       Parse card, put value in hdr.
 80         CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
            IF (END) GO TO 100
            ICARD = ICARD + 1
 90         CONTINUE
C                                       End card found.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
 100     IN = KINAX
         IS = KRCIC
         IE = IS + CATBLK(KIDIM) - 1
         DO 200 IAX = IS,IE
            IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1))
     *         CATR(IAX) = 1.0
            IN = IN + 1
 200        CONTINUE
         END IF
C                                       Correct for PDP 11 values.
C                                       set common values
      IF (FUCKUP) THEN
         DO 310 I = 3,7
            IDEPTH(I-2) = 1
            IF (I.GT.CATBLK(KIDIM)) GO TO 310
               IDEPTH(I-2) = CATR(KRCRP+I-1) + 0.5
               IDEPTH(I-2) = MAX (1, MIN (IDEPTH(I-2),
     *            CATBLK(KINAX+I-1)))
 310        CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
C                                       do conversion
         IF ((ABS(POS11(1)-CATD(KDCRV+KLOCL(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCL(LOCNUM)))) .OR.
     *      (ABS(POS11(2)-CATD(KDCRV+KLOCM(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCM(LOCNUM))))) THEN
            CALL LMPIX (POS11(1), POS11(2), PIX11(1), PIX11(2))
            IF ((ABS(PIX11(1)-CATR(KRCRP+KLOCL(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCL(LOCNUM))/2) .OR.
     *         (ABS(PIX11(2)-CATR(KRCRP+KLOCM(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCM(LOCNUM))/2)) THEN
               WRITE (MSGTXT,1310) POS11
               CALL MSGWRT (6)
               WRITE (MSGTXT,1311) PIX11
               CALL MSGWRT (6)
            ELSE
               WRITE (MSGTXT,1320) CATR(KRCRP+KLOCL(LOCNUM)),
     *            CATR(KRCRP+KLOCM(LOCNUM)), PIX11
               CALL MSGWRT (3)
               CATR(KRCRP+KLOCL(LOCNUM)) = PIX11(1)
               CATD(KDCRV+KLOCL(LOCNUM)) = POS11(1)
               CATR(KRCRP+KLOCM(LOCNUM)) = PIX11(2)
               CATD(KDCRV+KLOCM(LOCNUM)) = POS11(2)
               END IF
            END IF
         END IF
      GO TO 999
C                                       No Data, no tables.
 980  WRITE (MSGTXT,1980)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP CREATE ERROR')
 1310 FORMAT ('PDP11/70 ERROR: PHASE REF. POS.',2E14.6)
 1311 FORMAT ('GIVES REF. PIXEL',2F9.2,' IGNORED')
 1320 FORMAT ('CORRECTING REF. PIXEL FROM',2F8.2,' TO',2F8.2)
 1980 FORMAT ('THIS FITS FILE HAS NEITHER DATA NOR TABLES')
      END
C
      SUBROUTINE IMPARS (ICARD, FITBLK, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error, -1 => special header
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, NPNTS
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CONTINUE
      CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (IERR.NE.0) GO TO 999
      IF (FIRST) ISHIST = LHIST
C      IF ((ISHIST) .AND. (XERR4.GT.1.5)) GO TO 999
      FIRST = .FALSE.
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
      NPNTS = NPNT
      GO TO (100, 200, 300), KT
C                                       Logical value
 100     CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.GE.0) GO TO 110
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
C                                       Logical value special cases.
 110     CONTINUE
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
         GO TO 400
C                                       Number
 200     CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) GO TO 975
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).NE.'BLANK') GO TO 220
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
            GO TO 400
C                                       PDP 11 Stuff
 220     IF ((AWORD(TABNO).NE.'OPHRAE11') .AND.
     *      (AWORD(TABNO).NE.'OPHDCE11')) GO TO 230
            POS11(IPOFF) = X
            FUCKUP = .TRUE.
            GO TO 400
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
 230     IF (NBYT.NE.2) GO TO 240
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
            GO TO 400
C                                       4-byte real
 240     IF (NBYT.NE.4) GO TO 250
            IF ((AWORD(TABNO) .EQ. 'HISTORY') .AND. (X .GT. 1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
            GO TO 400
C                                       8-byte real
 250     IF (NBYT.NE.8) GO TO 400
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
            GO TO 400
C                                       String
 300     CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).NE.'IMCLASS') GO TO 320
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
            GO TO 400
C                                       Start string on real boundary.
 320     IPOFF = (NBYT / 4) * IPOFF
            CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
            CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
C
C                                       If this is a history card, look
C                                       for more values.
 400     IF (ISHIST) GO TO 10
         GO TO 999
C
 975  MSGTXT = 'IMPARS: NUMBER VALUE ERROR ON ' // SYMBOL
C                                       Error message
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (2A4,'LOGICAL VARIABLE HAS ILLEGAL VALUE')
      END



