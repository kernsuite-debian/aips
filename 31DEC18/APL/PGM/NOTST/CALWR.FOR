LOCAL INCLUDE 'CALWR.INC'
C                                                          Include CALWR
C                                       Local include for CALWR
      HOLLERITH XOBJ(2), XBAND
      CHARACTER NAME*12, CLASS*6, TYPE*2, OUTFIL*48, LINE*2880
      REAL      BLC(7), TRC(7)
      INTEGER   IVOL, ISEQ, ICNO, FDVEC(50), TBIND, DLUN, DIND, USER,
     *   ICARD, FITBLK(1440), TABLES, ANTFIL, IFLAG(20), IFMTYP,
     *   KLOCWT, NUMCOR, CATSAV(256), SCRTCH(256)
      LOGICAL   LSTOKE, ISCMP
      COMMON /INPARM/ XOBJ, XBAND, BLC, TRC
      COMMON /MORPRM/ CATSAV, FITBLK, LSTOKE, ISCMP, IVOL, ICNO, ISEQ,
     *   FDVEC, TBIND, DLUN, DIND, USER, ICARD,TABLES, ANTFIL, IFLAG,
     *   IFMTYP, KLOCWT, NUMCOR, SCRTCH
      COMMON /FTPCHR/ NAME, CLASS, TYPE, OUTFIL, LINE
C                                                          End CALWR.
LOCAL END
LOCAL INCLUDE 'CALWR2.INC'
C                                       Local include for buffers
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   TAPBUF(29184), IBUFSZ
      REAL      BUFF(UVBFSS), TBUFF(UVBFSS)
      COMMON /BUFRS/ TAPBUF, BUFF, TBUFF, IBUFSZ
LOCAL END
      PROGRAM CALWR
C-----------------------------------------------------------------------
C! Translate AIPS image file to a FITS-format file for calibrator set
C# Tape Map
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2007, 2013, 2015
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
C     This program will write an image of a calibrator source to a FITS
C   disk file suitable for standard calibrator usage.
C   Inputs:
C     INNAME     R(3)   Name of input file.
C     INCLASS    R(2)   Class of file.
C     INSEQ      R      sequence number of file.
C     INDISK     R      disk volume no. of file.
C     BLC        R(7)   Bottom left corner
C     TRC        R(7)   Top right corner
C     OUTFILE    R(12)  Disk file name
C-----------------------------------------------------------------------
      INTEGER   IERR, IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Set No. of files & no. of
C                                       scratch files to zero
      NCFILE = 0
      NSCR = 0
C                                       Get input parameters. Fill in
C                                       defaults.
      CALL CALWIN (RQUICK, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       write out image
      CALL CALWRI (IRET)
      CALL MAPCLS ('READ', IVOL, ICNO, DLUN, DIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
C                                       Then close it
      CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
C
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE CALWIN (RQUICK, IRET)
C-----------------------------------------------------------------------
C   CALWIN does the most basic inits for CALWR.  Get the parameters,
C   restart AIPS if required, fill in defaults, init the tape I/O.
C   Outputs: RQUICK  L         T -> AIPS already restarted
C            IRET    I         Return code : 0 => ok
C                                       else quit
C            COMMON /INPARM/
C            COMMON /MORPRM/
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   IRET
C
      CHARACTER PRGNAM*6, OBJECT*8, BAND*4
      INTEGER   NPARM, IERR, I, ITRIM
      HOLLERITH HFDVEC(50)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (HFDVEC, FDVEC)
      DATA PRGNAM, NPARM /'CALWR ', 17/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Zero fill FDVEC
      CALL FILL (50, 0, FDVEC)
C                                       Init the FITS I/O
      TBIND = 1
C                                       Initialize for AIPS
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XOBJ, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Convert characters
      CALL H2CHR (8, 1, XOBJ, OBJECT)
      CALL H2CHR (4, 1, XBAND, BAND)
      I = ITRIM (OBJECT)
      NAME = OBJECT(:I) // '_' // BAND
      I = ITRIM (NAME)
      CLASS = 'MODEL'
      ISEQ = 1
      IVOL = 0
      OUTFIL = 'FITS:' // NAME(:I) // '.MODEL'
      USER = NLUSER
      DLUN = 16
      WRITE (MSGTXT,1010) OUTFIL
      CALL MSGWRT (4)
      CALL CHR2H (48, OUTFIL, 1, HFDVEC(7))
      FDVEC(1) = 25
C                                       open output file
      TYPE = 'MA'
      CALL MAPOPN ('READ', IVOL, NAME, CLASS, ISEQ, TYPE, USER, DLUN,
     *   DIND, ICNO, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Force window
      BLC(1) = MIN (BLC(1), CATBLK(KINAX)+1-TRC(1))
      BLC(2) = MIN (BLC(2), CATBLK(KINAX+1)+1-TRC(2))
      TRC(1) = MAX (TRC(1), CATBLK(KINAX)+1-BLC(1))
      TRC(2) = MAX (TRC(2), CATBLK(KINAX+1)+1-BLC(2))
C                                       Open output file
      FDVEC(5) = 1
      FDVEC(2) = 2880
      FDVEC(3) = (29184 * NBITWD) / 8
      IFMTYP = 3
      FDVEC(6) = 1
      CALL TAPIO ('OPWT', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
      ELSE
         GO TO 999
         END IF
      GO TO 999
C                                       error close downs
 960  CALL MAPCLS ('READ', IVOL, ICNO, DLUN, DIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
C
 995  IRET = 16
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET PARMS.  IER=',I3)
 1010 FORMAT ('Writing to disk file: ',A)
 1020 FORMAT ('COULD NOT OPEN OUTPUT FILE.  IER=',I4)
      END
      SUBROUTINE CALWRI (IERR)
C-----------------------------------------------------------------------
C   CALWRI calls the specific routines for translating an AIPS MAp
C   image to FITS format.
C   COMMON /CFTP/
C      DOALL        R       < 0 (False) do first match, >= 0 (True)
C                           do every match.
C      NAME         C*12    Image designation (Name)
C      CLASS        C*6     Image designation (Class)
C      INSEQ        R       Image designation (Seq #)
C      INDISK       R       Inputs disk unit #
C      OUTTAPE      R       Output tape unit #
C   OUTPUT:
C      IERR         I       0=ok, 1=not ok.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, IBLC(7), ITRC(7)
      CHARACTER CHTMP*18
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IFMTYP = 3
      DO 10 I = 1,7
         IBLC(I) = BLC(I) + 0.01
         ITRC(I) = TRC(I) + 0.01
         IBLC(I) = MAX (1, IBLC(I))
         ITRC(I) = MAX (ITRC(I), IBLC(I))
 10      CONTINUE
      CALL COPY (256, CATBLK, CATSAV)
      CALL SUBHDR (BLC, TRC, 0.0, 0.0)
C                                       Tell user
      WRITE (MSGTXT,1000) USER, IVOL
      CALL H2CHR (18, 1, CATH(KHIMN), CHTMP)
      CALL NAMEST (CHTMP, CATBLK(KIIMS), MSGTXT(40:80), I)
      CALL MSGWRT (3)
C                                       Convert standard header to FITS
      CALL FITHCN (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'BASIC FITS HEADER', IERR
         GO TO 960
         END IF
C                                       Copy header extra keywords
      CALL FITKEY (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'HEADER KEYWORDS', IERR
         GO TO 960
         END IF
C                                       Copyright as history
      CALL FTMAHI (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'HISTORY', IERR
         GO TO 960
         END IF
C                                       Write mapdata onto tape
      CALL FITDCN (BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'BINARY IMAGE DATA', IERR
         GO TO 960
         END IF
C                                       Write ext files to tape
      CALL FITEXT (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'EXTENSION FILES', IERR
         CALL MSGWRT (6)
         END IF
C                                       Close Tape EOF
      MSGTXT = 'Writing end-of-file marks'
      CALL MSGWRT (2)
      CALL TAPIO ('FLSH', FDVEC, TAPBUF, TBIND, IERR)
      GO TO 999
C
C                                       Error return
 960  CALL MSGWRT (8)
      I = 1
      CALL ZTPCLS (FDVEC(1), FDVEC(40), IERR)
      FDVEC(40) = 0
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Writing image: User',I5,'  Disk',I2,'  Name')
 1010 FORMAT ('ERROR WRITING ',A,':',I7)
      END
      SUBROUTINE FITHCN (IERR)
C-----------------------------------------------------------------------
C   FITHCN converts the standard map header into the FITS format and
C   writes this header on tape.
C   Outputs:
C      IERR   I   Error return: 0--> okay, else  from IO routines
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER CHEXTN*8, TCOM*32, CHBLOK*8, BLKCOM*32, BCOM*32
      INTEGER   I, NWORDS, LBPX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DKEY.INC'
      INCLUDE 'INCS:VKEY.INC'
      DATA CHEXTN /'EXTEND  '/
      DATA TCOM /'Tables following main image     '/
      DATA CHBLOK, BLKCOM /'BLOCKED ', 'Tape may be blocked      '/
C-----------------------------------------------------------------------
C                                       Initialize
      ICARD = 0
      BCOM = '          '
C                                       Required keywords
      CALL KEYWRD (RWORD(1), RTYPE(1), RPOINT(1), BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       header no longer has bits/pix
      LBPX = -32
      CALL KEYWRD (RWORD(2), 10, LBPX, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      NWORDS = 3 + CATBLK(KIDIM)
      DO 45 I = 3,NWORDS
         CALL KEYWRD (RWORD(I), RTYPE(I), RPOINT(I), BCOM, IERR)
         IF (IERR.NE.0) GO TO 999
 45      CONTINUE
C                                       New FITS extension/table file.
      CALL KEYWRD (CHEXTN, 1, 1, TCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       New FITS BLOCKED = T
      CALL KEYWRD (CHBLOK, 1, 1, BLKCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Normal keywords
      DO 50 I = 1,NNT
         CALL KEYWRD (NWORD(I), NTYPE(I), NPOINT(I), NCOM(I), IERR)
         IF (IERR.NE.0) GO TO 999
 50      CONTINUE
C                                       Axis keywords
      NWORDS = 5 * CATBLK(KIDIM)
      DO 65 I = 1,NWORDS
         CALL KEYWRD (AWORD(I), ATYPE(I), APOINT(I), BCOM, IERR)
         IF (IERR.NE.0) GO TO 999
 65      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FITKEY (IERR)
C-----------------------------------------------------------------------
C   FITKEY writes history records into FITS headers containing all of
C   the special keywords from the catalog header.  So that readers will
C   recognize them they go out in the form:
C   HISTORY AIPS HEADERi keyword = value / comment
C   where i is the keyword type code (1-5 => double precision, single
C   precision, character*8, integer, logical).
C   Output:
C      IERR    I      Error code from IO routines.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   MXKEYS
      PARAMETER (MXKEYS=500)
      INTEGER   LOCS(MXKEYS), KEYTS(MXKEYS), ITEMP, IKEY, IC, NUMKEY, I,
     *   ITRIM
      CHARACTER KEYWOR(MXKEYS)*8, HCOM*27, CARD*80, HKEY*19, LOGKEY*1,
     *   DATEST*12
      LOGICAL   LTEMP
      REAL      VALUES(MXKEYS), RTEMP(2)
      DOUBLE PRECISION DTEMP
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (DTEMP, RTEMP, LTEMP, ITEMP)
      DATA HCOM /'AIPS Catalog Header Keyword'/
      DATA HKEY /'HISTORY AIPS HEADER'/
C-----------------------------------------------------------------------
C                                       read all header keywords
      NUMKEY = MXKEYS
      CALL CATKEY ('ALL ', IVOL, ICNO, KEYWOR, NUMKEY, LOCS,
     *   VALUES, KEYTS, BUFF, IERR)
      IF (IERR.NE.0) NUMKEY = 0
C                                       If any keywords read
      IF (NUMKEY.GT.0) THEN
         DO 20 IKEY = 1,NUMKEY
            IF ((KEYWOR(IKEY).NE.'ACTNOISE') .AND.
     *         (KEYWOR(IKEY).NE.'CCFLUX')) GO TO 20
C                                       Double precsion keyword
            IF (KEYTS(IKEY).EQ.1) THEN
               CALL RCOPY (NWDPDP, VALUES(LOCS(IKEY)), RTEMP)
               WRITE (CARD,1010) HKEY, KEYTS(IKEY), KEYWOR(IKEY), DTEMP,
     *            HCOM
C                                       Single precsion keyword
            ELSE IF (KEYTS(IKEY).EQ.2) THEN
               WRITE (CARD,1010) HKEY, KEYTS(IKEY), KEYWOR(IKEY),
     *            VALUES(LOCS(IKEY)), HCOM
C                                       Character keyword
            ELSE IF (KEYTS(IKEY).EQ.3) THEN
               CALL H2CHR (8, 1, VALUES(LOCS(IKEY)), DATEST)
               IF (INDEX(KEYWOR(IKEY), 'DATE').GT.0)
     *            CALL DATFST ('L2F', DATEST)
               I = MAX (8, ITRIM (DATEST))
               WRITE (CARD,1030) HKEY, KEYTS(IKEY), KEYWOR(IKEY),
     *            DATEST(:I), HCOM
C                                       Integer keyword
            ELSE IF (KEYTS(IKEY).EQ.4) THEN
               CALL RCOPY (1, VALUES(LOCS(IKEY)), RTEMP)
               WRITE (CARD,1040) HKEY, KEYTS(IKEY), KEYWOR(IKEY), ITEMP,
     *            HCOM
C                                       Logical keyword
            ELSE IF (KEYTS(IKEY).EQ.5) THEN
               CALL RCOPY (1, VALUES(LOCS(IKEY)), RTEMP)
               LOGKEY = 'F'
               IF (LTEMP) LOGKEY = 'T'
               WRITE (CARD,1050) HKEY, KEYTS(IKEY), KEYWOR(IKEY),
     *            LOGKEY, HCOM
C                                       shouldn't get here
            ELSE
               WRITE (MSGTXT,1000) IKEY, KEYTS(IKEY)
               CALL MSGWRT (6)
               GO TO 20
               END IF
C                                       Put card into buffer.
            IF (ICARD.GE.36) THEN
               CALL WRCTAP (IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            ICARD = ICARD + 1
C                                       Put card in buffer.
            IC = 80 * (ICARD - 1)  +  1
            CALL FITCHM (CARD, IC, FITBLK)
 20         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITKEY: KEYWORD',I3,' STRANGE TYPE =',I5)
 1010 FORMAT (A,I1,2X,A8,' =',1PE17.9,' /',A)
 1030 FORMAT (A,I1,2X,A8,' = ''',A,'''',6X,' /',A)
 1040 FORMAT (A,I1,2X,A8,' =',I12,5X,' /',A)
 1050 FORMAT (A,I1,2X,A8,' = ',A1,15X,' /',A)
      END
      SUBROUTINE FTMAHI (IERR)
C-----------------------------------------------------------------------
C   FTMAHI writes copyright as history records by calling FITHIS and
C   then adding special records re Clean.
C   Output:
C      IERR    I      Error code from IO routines.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER PRODS(5)*12
      INTEGER   I, J, IC, IL
      REAL      X, Y, Z
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRODS /'NORMAL      ', 'COMPONENTS  ',
     *   'RESIDUAL    ', 'POINTS      ', 'DIRTY MAP   '/
C-----------------------------------------------------------------------
C                                       Copy HI file + general AIPS
      CALL FITHIS (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write MAp specific 'AIPS' HI
C                                       Clean parameters
C                                       Convert parameters
      X = CATR(KRBMJ)
      Y = CATR(KRBMN)
      Z = CATR(KRBPA)
      IF ((CATBLK(KINIT).GT.0) .OR. (X.GT.0.) .OR. (Y.GT.0.) .OR.
     *   (Z.NE.0.)) THEN
C                                       Clean beam
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         WRITE (MSGTXT,1020) X, Y, Z
         CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       Iterations, product
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         I = MAX (1, MIN (4, CATBLK(KITYP)))
         J = I
         IF (CATBLK(KINIT).LE.0) I = 5
         IF (CATBLK(KINIT).LE.0) J = 0
         WRITE (MSGTXT,1025) CATBLK(KINIT), J, PRODS(I)
         CALL FITCHM (MSGTXT, IC, FITBLK)
         END IF
C                                       imaging type, xpoff, ypoff
      I = CATBLK(KIITY)
      IF ((I.EQ.1) .OR. (I.EQ.2)) THEN
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         WRITE (MSGTXT,1030) I, CATR(KRXPO), CATR(KRYPO)
         CALL FITCHM (MSGTXT, IC, FITBLK)
         END IF
C                                       END card
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      IC = ICARD * 80 + 1
      MSGTXT = 'END     '
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       Fill and Write record
      IL = 2880 - IC + 1
      IF (IL.GT.0) THEN
         LINE (IC:2880) = '                  '
         CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
         END IF
      IL = 2880 / (NBITWD / 8)
      CALL COPY (IL, FITBLK, TAPBUF(TBIND))
      CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('HISTORY AIPS   CLEAN BMAJ=',1PE12.4,' BMIN=',1PE12.4,
     *   ' BPA=',0PF7.2)
 1025 FORMAT ('HISTORY AIPS   CLEAN NITER=',I9,' PRODUCT=',I1,3X,
     *   '/ ',A)
 1030 FORMAT ('HISTORY AIPS   IMAGE ITYPE=',I1,' XPOFF=',1PE16.8,
     *   ' YPOFF=',1PE16.8)
      END
      SUBROUTINE FITDCN (NBLC, NTRC, IER)
C-----------------------------------------------------------------------
C   FITDCN reads the standard map image data and writes the image.
C   Inputs:
C      NBLC     R(7)    Bottom left corner
C      NTRC     R(7)    Top right corner
C   Outputs:
C      IER   I     Error return
C                     0--> okay
C                     1--> error condition
C-----------------------------------------------------------------------
      INTEGER   IER
      REAL      NBLC(7), NTRC(7)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IERR, IWIN(4), NBKOF1, IOFF, NX, NY, IDEPTH(5), NBYB, I,
     *   INX, INY, IBL, NVAL, ININD, NXX, NXY, I3, I3A, I3B, I4, I4A,
     *   I4B, I5, I5A, I5B, I6, I6A, I6B, I7, I7A, I7B
      REAL      BUFFRR(MAXIMG), FITBRR(1440)
      DOUBLE PRECISION NONZER, NZERO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (FITBLK(1), FITBRR(1))
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      NBYB = UVBFSS * 2
      NONZER = 0.0D0
      NZERO = 0.0D0
      NVAL = 720
      CALL FILL (1440, 0, FITBLK)
      MSGTXT = 'Now writing the image'
      CALL MSGWRT (3)
C                                       Set window parms
      I3A = NBLC(3) + 0.01
      I4A = NBLC(4) + 0.01
      I5A = NBLC(5) + 0.01
      I6A = NBLC(6) + 0.01
      I7A = NBLC(7) + 0.01
      I3B = NTRC(3) + 0.01
      I4B = NTRC(4) + 0.01
      I5B = NTRC(5) + 0.01
      I6B = NTRC(6) + 0.01
      I7B = NTRC(7) + 0.01
      IWIN(1) = NBLC(1) + 0.01
      IWIN(2) = NBLC(2) + 0.01
      IWIN(3) = NTRC(1) + 0.01
      IWIN(4) = NTRC(2) + 0.01
      NY = IWIN(4) - IWIN(2) + 1
      NX = IWIN(3) - IWIN(1) + 1
      INX = CATSAV(KINAX)
      INY = CATSAV(KINAX+1)
      IBL = 0
      DO 100 I7 = I7A,I7B
      DO 100 I6 = I6A,I6B
      DO 100 I5 = I5A,I5B
      DO 100 I4 = I4A,I4B
      DO 100 I3 = I3A,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATSAV(KIDIM), CATSAV(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         CALL MINIT ('READ', DLUN, DIND, INX, INY, IWIN, BUFF, NBYB,
     *      NBKOF1, IERR)
         IF (IERR.EQ.0) GO TO 40
            WRITE (MSGTXT,1030) IERR
            GO TO 980
C                                       Begin read/write loop
 40      DO 90 I = 1,NY
C                                       Read a map line
            CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, IERR)
            IF (IERR.EQ.0) GO TO 50
               WRITE (MSGTXT,1040) IERR,I
               GO TO 980
 50         CALL RCOPY (NX, BUFF(ININD), BUFFRR)
            NXY = NX
            IOFF = 1
 55         NXX = MIN (NXY, NVAL-IBL)
C                                       Need new buffer
            IF (NXX.LE.0) THEN
               CALL WRTAPE (IBL, IERR)
               IBL = 0
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1055) IERR
                  GO TO 980
                  END IF
               GO TO 55
               END IF
C                                       Do copy
            CALL RCOPY (NXX, BUFFRR(IOFF), FITBRR(1+IBL))
            IBL = IBL + NXX
            IOFF = IOFF + NXX
            NXY = NXY - NXX
            IF (NXY.GT.0) GO TO 55
 90         CONTINUE
 100     CONTINUE
C                                       Warning:
      IF (NZERO.GT.0.5D0) THEN
         NZERO = 100.0D0 * NZERO / (NZERO + NONZER)
         WRITE (MSGTXT,1100) NZERO
         IF (NZERO.GT.0.1D0) CALL MSGWRT (6)
         END IF
C                                       Write last record on tape
      IF (IBL.LE.0) GO TO 999
         CALL WRTAPE (IBL, IERR)
         IF (IERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1055) IERR
            GO TO 980
C
 980  CALL MSGWRT (8)
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('FITDCN: COULD NOT INITIALIZE MAP.  IER=',I4)
 1040 FORMAT ('FITDCN: COULD NOT READ MAP.  IER=',I3,' LINE=',I4)
 1055 FORMAT ('FITDCN: COULD NOT WRITE FITS RECORD.  IER=',I4)
 1100 FORMAT ('WARNING: ',F8.4,' per cent of pixels written as 0')
      END
      SUBROUTINE KEYWRD (WORD, ITYPE, POINT, COMENT, IERR)
C-----------------------------------------------------------------------
C   This routine will encode a given keyword and the corresponding
C   value in the header into the FITS card format and put that card
C   into the tape output buffer.  If that buffer is full, the output
C   buffer will be written to tape.
C   INPUTS:
C      WORD    C*8     FITS keyword (characters).
C      ITYPE   I        data type of header value.
C                           1=Logical variable
C                           2=Number
C                           3=String on real boundary
C                           4=String on integer boundary
C                           5=image scaling parms
C                           6=Rand parm scalling factors.
C                           7=Rand parm offsets.
C                           8=Rand parm names.
C                           9=blanking value.
C                          10=value = pointer.
C                          11=NUMERIC, omit if zero.
C      POINT   I        'pointer code' of the header value, i.e. 1000 *
C                       length in bytes + 100 * offset into the header +
C                       position of pointer in common HDRVAL.
C      COMENT  C*32     comment for this card.
C      COMMON /MAPHDR/
C      COMMON /HDRVAL/
C      COMMON /MORPRM/
C   IN/OUT: (In common)
C      ICARD   I         last card in the buffer FITBLK (0-36).
C      TBIND   I         Starting word of current buffer in FITBLK.
C      FITBLK  I(1440)   output work buffer.
C      TAPBUF  I(*)      TAPIO buffers
C   OUTPUT:
C      IERR    I         error code. 0=ok, 1=bad.
C-----------------------------------------------------------------------
      CHARACTER WORD*8, COMENT*32, CARD*80, STR*12
      INTEGER   ITYPE, POINT, IERR
      INTEGER   KPNTR(58), PNTR, POFF, NBYT, ISTART, IC, ITEND, ITRIM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KPNTR, KHOBJ)
C-----------------------------------------------------------------------
C                                       Decode POINT.
      IERR = 0
      IF ((ITYPE.LT.1) .OR. (ITYPE.GT.11)) GO TO 999
      PNTR = MOD (POINT, 1000)
      POFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = POINT / 1000
C                                       Go to appropiate data type.
C                                       Special cases start at label
C                                       600.
      GO TO (100, 200, 300, 400, 500, 600, 610, 620, 630, 640,
     *   200), ITYPE
C                                       Logical (always true so far)
 100     WRITE (CARD,1100) WORD, COMENT
         GO TO 800
C                                       Numeric.
C                                       Integer.
 200     IF (NBYT.NE.2) GO TO 210
            IF ((ITYPE.EQ.11) .AND. (CATBLK(PNTR+POFF).EQ.0)) GO TO 999
            WRITE (CARD,1200) WORD, CATBLK(PNTR+POFF), COMENT
            GO TO 800
C                                       Real
 210     IF (NBYT.NE.4) GO TO 220
            IF ((ITYPE.EQ.11) .AND. (CATR(PNTR+POFF).EQ.0.0)) GO TO 999
            WRITE (CARD,1210) WORD, CATR(PNTR+POFF), COMENT
            GO TO 800
C                                       Double precision
 220        IF ((ITYPE.EQ.11) .AND. (CATD(PNTR+POFF).EQ.0.0D0))
     *         GO TO 999
            WRITE (CARD,1220) WORD, CATD(PNTR+POFF), COMENT
            GO TO 800
C                                       Character on real boundary.
 300        WRITE (CARD,1300) WORD, COMENT
C                                       Dates are special
            IF (WORD(:4).EQ.'DATE') THEN
               ISTART = NBYT * POFF  +  1
               STR  = ' '
               CALL H2CHR (NBYT, ISTART, CATH(PNTR), STR)
               CALL DATFST ('L2F', STR)
               NBYT = ITRIM (STR)
               CARD(12:11+NBYT) = STR(:NBYT)
            ELSE
               ISTART = NBYT * POFF  +  1
               CALL H2CHR (NBYT, ISTART, CATH(PNTR), CARD(12:11+NBYT))
               END IF
            ITEND = NBYT + 12
            CARD(ITEND:ITEND) = ''''
            GO TO 800
C                                       Character on integer boundary.
 400        WRITE (CARD,1300) WORD, COMENT
            ITEND = NBYT + 12
            ISTART = NBYT * POFF  +  1
            CALL H2CHR (NBYT, ISTART, CATH(PNTR), CARD(12:11+NBYT))
            CARD(ITEND:) = ''''
            GO TO 800
C                                       Image scaling parameter.
 500        IF (WORD.EQ.'BSCALE') WRITE (CARD,1220) WORD, 1.0D0, COMENT
            IF (WORD.EQ.'BZERO') WRITE (CARD,1220) WORD, 0.0D0, COMENT
            GO TO 800
C                                       Get scaling factors from
C                                       common MORPRM.
 600        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, 1.0D0, COMENT
            GO TO 800
C                                       Get offsets from MORPRM.
 610        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, 0.0D0, COMENT
            GO TO 800
C                                       Get RP type from MORPRM
 620        PNTR = MOD (POINT, 100)
            WRITE (CARD,1300) WORD, COMENT
            ITEND = NBYT + 12
            CARD(12:11+NBYT) = ' '
            CARD(ITEND:) = ''' '
            GO TO 800
C                                       Blanking
 630        IF (CATR(PNTR+POFF).EQ.0) GO TO 999
            IF (IFMTYP.EQ.1) WRITE (CARD,1630) WORD
            IF (IFMTYP.EQ.2) WRITE (CARD,1631) WORD
            IF (IFMTYP.EQ.3) WRITE (CARD,1632)
            GO TO 800
C                                       Value given by POINT.
 640        WRITE (CARD,1200) WORD, POINT, COMENT
C                                       Put card into buffer.
 800  IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
C                                       Put card in buffer.
      IC = 80 * (ICARD - 1)  +  1
      CALL FITCHM (CARD, IC, FITBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A8,'= ',19X,'T /',A)
 1200 FORMAT (A8,'= ',I20,' /',A)
 1210 FORMAT (A8,'= ',1PE20.9,' /',A)
 1220 FORMAT (A8,'= ',1PE20.11,' /',A)
 1300 FORMAT (A8,'= ''',19X,' /',A)
 1630 FORMAT (A8,'= ',14X,'-32768 / Blanked pixel tape value')
 1631 FORMAT (A8,'= ',9X,'-2147483648 / Blanked pixel tape value')
 1632 FORMAT ('COMMENT / IEEE not-a-number used for blanked',
     *   ' f.p. pixels')
      END
      SUBROUTINE FITHIS (IERR)
C-----------------------------------------------------------------------
C   This routine will write the general history information to tape.
C   An END card is NOT written.
C   In/out:  (in common)
C      ICARD    I         The last card written in FITBLK  (1:36)
C      TBIND    I         TAPBUF(TBIND) is the start of the current I/O
C                         buffer.
C      FITBLK   I(1440)   header buffer
C      TAPBUF   I(*)      TAPIO buffers
C   Output:
C      IERR     I         error code. 0=ok, 1=bad>
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   NGNU
      PARAMETER (NGNU = 26)
      CHARACTER COFITS(2)*72, GPLTXT(2,NGNU)*35, GPLTX1(2,10)*35,
     *   GPLTX2(2,10)*35, GPLTX3(2,NGNU-20)*35, CTEMP*4
      HOLLERITH IBUFF(256)
      INTEGER   I, IC, ID(3)
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBUFF, BUFF)
      EQUIVALENCE (GPLTXT(1,1), GPLTX1(1,1))
      EQUIVALENCE (GPLTXT(1,11), GPLTX2(1,1))
      EQUIVALENCE (GPLTXT(1,21), GPLTX3(1,1))
      DATA GPLTX1 /'-----------------------------------',
     *             '-----------------------------------',
     *             '  Copyright (C)                    ',
     *             '                                   ',
     *             '  Associated Universities, Inc. Was',
     *             'hington DC, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is free software; yo',
     *             'u can redistribute it and/or       ',
     *             '  modify it under the terms of the ',
     *             'GNU General Public License as      ',
     *             '  published by the Free Software Fo',
     *             'undation; either version 2 of      ',
     *             '  the License, or (at your option) ',
     *             'any later version.                 ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is distributed in th',
     *             'e hope that it will be useful,     '/
      DATA GPLTX2 /'  but WITHOUT ANY WARRANTY; without',
     *             ' even the implied warranty of      ',
     *             '  MERCHANTABILITY or FITNESS FOR A ',
     *             'PARTICULAR PURPOSE.  See the       ',
     *             '  GNU General Public License for mo',
     *             're details.                        ',
     *             '                                   ',
     *             '                                   ',
     *             '  You should have received a copy o',
     *             'f the GNU General Public           ',
     *             '  License along with this program; ',
     *             'if not, write to the Free          ',
     *             '  Software Foundation, Inc., 675 Ma',
     *             'ssachusetts Ave, Cambridge,        ',
     *             '  MA 02139, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  Correspondence concerning AIPS sh',
     *             'ould be addressed as follows:      '/
      DATA GPLTX3 /'         Internet email: aipsmail@n',
     *             'rao.edu.                           ',
     *             '         Postal address: AIPS Proje',
     *             'ct Office                          ',
     *             '                         National R',
     *             'adio Astronomy Observatory         ',
     *             '                         520 Edgemo',
     *             'nt Road                            ',
     *             '                         Charlottes',
     *             'ville, VA 22903-2475 USA           ',
     *             '-----------------------------------',
     *             '-----------------------------------'/
C-----------------------------------------------------------------------
      IERR = 0
      COFITS(1) = '  FITS (Flexible Image Transport System) format is'
     *   // ' defined in ''Astronomy'
      COFITS(2) = '  and Astrophysics'', volume 376, page 359;' //
     *   ' bibcode: 2001A&A...376..359H'
C                                       ORIGIN keyword
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      WRITE (MSGTXT,1000) HSTNAM, SYSNAM, RLSNAM
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       history: FITS ref 1 and 2
      DO 10 I = 1,2
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         MSGTXT = 'COMMENT ' // COFITS(I)
         CALL FITCHM (MSGTXT, IC, FITBLK)
 10      CONTINUE
C                                       copyleft
      CALL ZDATE (ID)
      IF (ID(1).LT.200) ID(1) = ID(1) + 1900
      WRITE (CTEMP,1010) ID(1)
      GPLTXT(1,2)(17:20) = CTEMP
      DO 20 I = 1,NGNU
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         MSGTXT = 'HISTORY ' // GPLTXT(1,I) // GPLTXT(2,I)
         CALL FITCHM (MSGTXT, IC, FITBLK)
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ORIGIN  = ''AIPS',A12,1X,A20,1X,A7,'''',4X,'/ ')
 1010 FORMAT (I4)
      END
      SUBROUTINE WRTAPE (ITFULL, IERR)
C-----------------------------------------------------------------------
C   Writes TAPBUF to tape and resets counters.
C   INPUTS:
C      ITFULL  I        Number values filled in buffer (will zero fill)
C   Inputs via common:
C      FDVEC   I(40)    File descriptor vector for TAPIO output
C      FITBLK  I(*)     header buffer
C      TAPBUF  I(*)     TAPIO buffers
C      IFMTYP  I        Desired format in FITBLK
C   In/Out via common:
C      TBIND   I      starting index of current buffer in TAPBUF.
C   Outputs:
C      IERR    I      IO error code.
C-----------------------------------------------------------------------
      REAL      FITBR4(1440)
      INTEGER   FITBI4(1440)
      INTEGER   ITFULL, ITMAX, IERR, I, ITZ
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBI4, FITBR4)
C-----------------------------------------------------------------------
C                                       Write this buffer to tape.
      ITMAX = 720
      IF (IFMTYP.EQ.1) ITMAX = 1440
      ITZ = ITMAX - ITFULL
      IF (ITZ.LE.0) GO TO 100
         GO TO (10, 30, 50), IFMTYP
 10      CONTINUE
            DO 20 I = 1,ITZ
               FITBLK(I+ITFULL) = 0
 20            CONTINUE
            GO TO 100
 30      CONTINUE
            DO 40 I = 1,ITZ
               FITBI4(I+ITFULL) = 0
 40            CONTINUE
            GO TO 100
 50      CONTINUE
            DO 60 I = 1,ITZ
               FITBR4(I+ITFULL) = 0.0
 60            CONTINUE
C                                       translate
 100  IF (IFMTYP.EQ.1) CALL ZILI16 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.2) CALL ZILI32 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.3) CALL ZRLR32 (ITMAX, 1, FITBLK, TAPBUF(TBIND))
C                                       write
      CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
      END
      SUBROUTINE FITCHM (TEXT, IC, BUFFER)
C-----------------------------------------------------------------------
C   FITCHM moves TEXT to BUFFER 80-charcter strings converting to
C   real world characters.
C   Inputs/outputs:
C      TEXT    C*80      text line:
C      IC      I          pointer to char loc for text
C      BUFFER  I(*)       buffer to hold n lines
C-----------------------------------------------------------------------
      CHARACTER TEXT*80
      INTEGER   IC, BUFFER(*)
C-----------------------------------------------------------------------
      CALL ZCLC8 (80, TEXT, IC, BUFFER)
      IC = IC + 80
C
 999  RETURN
      END
      SUBROUTINE FT2CHM (TEXT, ICARD, BUFFER)
C-----------------------------------------------------------------------
C   FT2CHM moves TEXT to BUFFER converting to real world characters.
C   Inputs/outputs:
C      TEXT     C*80   text line:
C      ICARD    I      pointer to char loc for text
C      BUFFER   I(*)   buffer to hold n lines
C-----------------------------------------------------------------------
      CHARACTER TEXT*80
      INTEGER   ICARD, BUFFER(*)
C
      INTEGER   IC
C-----------------------------------------------------------------------
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      CALL ZCLC8 (80, TEXT, IC, BUFFER)
C
 999  RETURN
      END
      SUBROUTINE WRCTAP (IERR)
C-----------------------------------------------------------------------
C   WRCTAP writes a tape record of FITS character data.
C   The data in FITBLK must already have been converted into real world
C   characters.
C   Output:  IERR   I     Error code of IO routines.
C-----------------------------------------------------------------------
      INTEGER   IERR, IL
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Copy to buffer
      IL = 2880 / (NBITWD/8)
      CALL COPY (IL, FITBLK, TAPBUF(TBIND))
      CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
      ICARD = 0
C
 999  RETURN
      END
      SUBROUTINE FITEXT (IER)
C-----------------------------------------------------------------------
C   FITEXT copies AIPS tables files to the FITS output file.  All
C   tables will be written as 3-D tables unless DOASC=.TRUE.; in this
C   case, all files which can be written as ASCII tables will be
C   written thus and others will be written as 3-D tables.
C   Output:  IER    I     Error number: 0 => none
C                           else count of errors or tape IO error #
C-----------------------------------------------------------------------
      INTEGER   IER
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TTYPE*2
      INTEGER   NEXTF, IVER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Loop for all extension files.
      CALL FXHDEX (CATBLK)
      NEXTF = KHEXT + KIEXTN - 1
      TTYPE = 'CC'
      IVER = 1
      CALL EXTWRT (TTYPE, IVER, IER)
C
 999  RETURN
      END
      SUBROUTINE EXTWRT (TTYPE, IVER, IER)
C-----------------------------------------------------------------------
C   EXTWRT copies a single, specified AIPS table file to the FITS
C   output file.  Table will be written as 3-D tables unless
C   DOASC=.TRUE.; in this case, a file which can be written as an ASCII
C   table will be  written thus else it will be written as a 3-D table.
C   Inputs:
C      TTYPE   C*2   Extension table type, 2 char.
C      IVER    I     Version number.
C   Output:
C      IER     I     Error number: 0 => none
C                       else count of errors or tape IO error #
C-----------------------------------------------------------------------
      INTEGER   IER
C
      INCLUDE 'INCS:PUVD.INC'
C                                       XBPRSZ is maximum size of an
C                                       array in a table record in real
C                                       words, worst case is bandpass
C                                       table.
      CHARACTER COLHDR(128)*24, UNITS(128)*8, KWNAME*8, TTYPE*2,
     *   FORM3D*8, FORMXX*8, FORCOD(7)*1, FORM(7)*8, TYPTAB(2)*8,
     *   FRMOUT*8, STARS*32, CHTMP*10
      HOLLERITH RECH(XBPRSZ), HVALUE(2)
      DOUBLE PRECISION RECORD(XBPRSZ/2), DVALUE, RECRD(XBPRSZ/2),
     *   DLINE(XBPRSZ/2)
      REAL      RLINE(XBPRSZ), RECRR(XBPRSZ), RVALUE
      INTEGER   NREC, RECI(XBPRSZ), IRNO, JVALUE,
     *   IPAIR, SUMBYT, MXTEST, MAXL, CH1, CHEND
      LOGICAL   EXIST, TABLE, RECL(XBPRSZ), FITASC, IS3D
      INTEGER   IVER, LUN, IFORMT(128), BUFFER(512), IC, I, II, ITRIM,
     *   IERR, IFLEN(128), ITLEN(7), ITYPE, NKEY, NCOL, DATP(128,2),
     *   ICRDL, INCRD, IL, IRCODE, IDUM,ILEN, IBCOL(128), MINLEN, IOFF,
     *   ILEN0, JJ, SRTORD, TBTYP, NUMFOR, NUMBYT(7),
     *   NCOPY, NEXT, TCOUNT(128), TPTYPE(128), TOFF(128),
     *   ITNCOL, FLAGD, RECOUT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALWR.INC'
      INCLUDE 'CALWR2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFF, BUFFER)
      EQUIVALENCE (DVALUE, HVALUE)
      EQUIVALENCE (DLINE, RLINE)
      EQUIVALENCE (RECORD, RECRD, RECRR, RECI, RECL, RECH)
      DATA FORCOD /'D','E','A','J','L','I','X'/
      DATA STARS /'********************************'/
      DATA NUMBYT / 8, 4,  1,  4,  1,  2,  1/
      DATA MAXL /XBPRSZ/
      DATA LUN /28/
      DATA FORM /'D24.15  ','E15.6   ','        ',
     *   'I12     ','A1      ', 'I6      ','        '/
      DATA ITLEN /24, 15, 0, 12, 0, 6, 0/
      DATA TYPTAB /'A3DTABLE',  'BINTABLE'/
C-----------------------------------------------------------------------
      IER = 0
      ILEN0 = 160
      IS3D = .TRUE.
C                                       See if we have a table.
      CALL ISTAB (TTYPE, IVOL, ICNO, IVER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IERR)
      IF (.NOT.EXIST) GO TO 999
      IF (.NOT.TABLE) GO TO 999
      CALL TABINI ('READ', TTYPE, IVOL, ICNO, IVER, CATBLK, LUN, NKEY,
     *   IDUM, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      NREC = BUFFER(5)
C                                       Count the unflagged records.
C                                         This will have to be revised.
      FLAGD = 0
      DO 10 IRNO = 1,NREC
         CALL TABIO ('READ', IRCODE, IRNO, RECI, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.-1)) GO TO 999
C                                       IERR=-1 is flagged row.
         IF (IERR.EQ.-1) FLAGD = FLAGD + 1
         IERR = 0
 10      CONTINUE
C                                       Let the user know.
C
      RECOUT = NREC - FLAGD
      IF (FLAGD.GT.0) THEN
         WRITE (MSGTXT,1010) RECOUT
         CALL MSGWRT (4)
         WRITE (MSGTXT,1011) NREC, TTYPE, IVER
         CALL MSGWRT (4)
         ENDIF
C                                       Everything you need to know
C                                       about the columns.
      CALL GETHUT (NCOL, DATP, BUFFER, COLHDR, UNITS,
     *   IFORMT, IFLEN, IERR)
C                                       Determine col start positions
C                                       and number of bytes for 3-D
      II = IFORMT(1)
      SUMBYT = IFLEN(1) * NUMBYT(II)
      IF (II.EQ.7) SUMBYT = 1 + (IFLEN(1)-1) / NBITWD
      IBCOL(1) = 1
      DO 40 I = 2,NCOL
         II = IFORMT(I-1)
         IBCOL(I) = IBCOL(I-1) + ITLEN(II) + IFLEN(I-1) + 1
         II = IFORMT(I)
         IF (II.NE.7) SUMBYT = SUMBYT + IFLEN(I) * NUMBYT(II)
         IF (II.EQ.7) SUMBYT = SUMBYT + 1 + (IFLEN(I)-1)/NBITWD
 40      CONTINUE
      II = IFORMT(NCOL)
      IF (II.EQ.7) MINLEN = IBCOL(NCOL) +
     *   (IFLEN(I-1) - 1) / 4 + 1
      IF (II.NE.7) MINLEN = IBCOL(NCOL) + ITLEN(II) + IFLEN(II)
C                                       Make card length a multiple of
C                                       60 to make things simple.
      ICRDL = 60 * (MINLEN / 60  +  1)
      INCRD = 2880 / ICRDL
C                                       We did not reserve enuf buffer
C                                       size in array LINE or RLINE
      MXTEST = (MAXL * 2)
      IF (SUMBYT.GT.MXTEST) THEN
         WRITE (MSGTXT,1040) ICRDL
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Table header
      ICARD = 0
C                                       Table type
      TBTYP = 2
C                                       Tell user table and type
      WRITE (MSGTXT,1015) TTYPE
      CALL MSGWRT (3)
      WRITE (MSGTXT,1050) TYPTAB(TBTYP)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1051)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1052)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1053) SUMBYT
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1054) RECOUT
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1055)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1056)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1057) NCOL
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1058) TTYPE
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      WRITE (MSGTXT,1059) IVER
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
      DO 80 I = 1,NCOL
         IF (I.LT.10) WRITE (MSGTXT,1060) I, IBCOL(I)
         IF (I.GE.10) WRITE (MSGTXT,1061) I, IBCOL(I)
C         IF (.NOT.IS3D) CALL FT2CHM (MSGTXT, ICARD, FITBLK)
         IF (ICARD.GT.35) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
         II = IFORMT(I)
         IF (II.NE.3) GO TO 70
C                                       Try to get character format
C                                       into the right form.
            WRITE (MSGTXT,1062) IFLEN(I)
            CALL CHTRIM (MSGTXT, 8, MSGTXT, ILEN)
            FORM(3) = 'A' // MSGTXT(1:ILEN)
 70      CONTINUE
C                                       Get correct format for file type
         FRMOUT = FORM(II)
C                                       3-D tables
         WRITE (FORM3D,1076) IFLEN(I), FORCOD(II)
         FORMXX = '        '
         CALL CHTRIM (FORM3D, 8, FORMXX, NUMFOR)
         FRMOUT = FORMXX
         IF (I.LT.10) WRITE (MSGTXT,1070) I, FRMOUT, I
         IF (I.GE.10) WRITE (MSGTXT,1071) I, FRMOUT, I
         CALL FT2CHM (MSGTXT, ICARD, FITBLK)
         IF (ICARD.GT.35) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
         IF (I.LT.10) WRITE (MSGTXT,1072) I, COLHDR(I)(1:16), I
         IF (I.GE.10) WRITE (MSGTXT,1073) I, COLHDR(I)(1:16), I
         CALL FT2CHM (MSGTXT, ICARD, FITBLK)
         IF (ICARD.GT.35) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
         IF (I.LT.10) WRITE (MSGTXT,1074) I, UNITS(I), I
         IF (I.GE.10) WRITE (MSGTXT,1075) I, UNITS(I), I
         CALL FT2CHM (MSGTXT, ICARD, FITBLK)
         IF (ICARD.GT.35) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
 80      CONTINUE
C                                       Do all KeyWord value pairs.
      NKEY = BUFFER(53)
      IF (NKEY.GE.1) THEN
         DO 240 IPAIR = 1,NKEY
            CALL GTPAIR (IPAIR, BUFFER, KWNAME, DVALUE, ITYPE)
            IF (ITYPE.EQ.0) GO TO 240
            GO TO (120, 140, 160, 180, 200), ITYPE
C                                       Double Precision.
 120           WRITE (MSGTXT,1120) KWNAME, DVALUE
               GO TO 230
C                                       Real.
 140           RVALUE = DVALUE
               WRITE (MSGTXT,1140) KWNAME, RVALUE
               GO TO 230
C                                       Character.
 160           CALL H2CHR (8, 1, HVALUE, CHTMP)
               IF (INDEX(KWNAME,'DATE').GT.0) CALL DATFST ('L2F', CHTMP)
               I = MAX (8, ITRIM (CHTMP))
               WRITE (MSGTXT,1160) KWNAME, CHTMP(:I)
               GO TO 230
C                                       Integer.
 180           IF (DVALUE.LT.0.0D0) JVALUE = DVALUE - 0.01D0
               IF (DVALUE.GE.0.0D0) JVALUE = DVALUE + 0.01D0
               WRITE (MSGTXT,1180) KWNAME, JVALUE
               GO TO 230
C                                       Logical.
 200           IF (DVALUE.GT.0.0D0) WRITE (MSGTXT,1200) KWNAME
               IF (DVALUE.LE.0.0D0) WRITE (MSGTXT,1202) KWNAME
               GO TO 230
C                                       Integer (short no longer valid)
C 220           IER = 5
C               MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
C               CALL MSGWRT (8)
C               GO TO 999
C
 230        CALL FT2CHM (MSGTXT, ICARD, FITBLK)
            IF (ICARD.GT.35) CALL WRCTAP (IERR)
 240        CONTINUE
         END IF
C                                       Sort order.
      SRTORD = BUFFER(43)
      IF (SRTORD.NE.0) THEN
         WRITE (MSGTXT,1230) SRTORD
         CALL FT2CHM (MSGTXT, ICARD, FITBLK)
         IF (ICARD.GT.35) CALL WRCTAP (IERR)
         END IF
C                                       END card.
      WRITE (MSGTXT,1240)
      CALL FT2CHM (MSGTXT, ICARD, FITBLK)
C                                       Blank fill rest of block.
      IC = 80 * ICARD  + 1
      IL = 2881 - IC
      IF (IL.GT.0) THEN
         LINE (IC:2880) = '                  '
         CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
         END IF
      CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Decide file type
      IF (IS3D) GO TO 500
C                                       ASCII format table
C                                       Table data
      IRCODE = 0
      DO 340 IRNO = 1,NREC
         CALL TABIO ('READ', IRCODE, IRNO, RECI, BUFFER, IERR)
         IF (IERR.EQ.-1) GO TO 340
            IF (IERR.EQ.0) GO TO 270
               WRITE (MSGTXT,1245) IERR
               CALL MSGWRT (6)
               DO 260 II = 1, NCOL
                  ITYPE = IFORMT(II)
                  ILEN = ITLEN(ITYPE) + IFLEN(II)
                  LINE(IBCOL(II):IBCOL(II)+ILEN-1) = STARS(1:ILEN)
 260              CONTINUE
               GO TO 320
 270        CONTINUE
            IF (ICARD.GE.INCRD) CALL WRCTAP (IERR)
            IF (IERR.NE.0) GO TO 900
            IC = ICRDL * ICARD + 1
            ICARD = ICARD + 1
            LINE(1:ICRDL) = '     '
C                                       Put each column value in the
C                                       card line.
            DO 300 II = 1,NCOL
               ITYPE = IFORMT(II)
               ILEN = ITLEN(ITYPE) + IFLEN(II)
               IOFF = DATP(II,1)
               CH1 = IBCOL(II)
               CHEND = CH1 + ILEN - 1
               GO TO (281, 282, 283, 284, 285, 286), ITYPE
C                                       Double precision.
 281              WRITE (LINE(CH1:CHEND),1281) RECRD(IOFF)
                  GO TO 295
C                                       Real.
 282              WRITE (LINE(CH1:CHEND),1282) RECRR(IOFF)
                  GO TO 295
C                                       Character
 283              CALL H2CHR (ILEN, 1, RECH(IOFF), LINE(CH1:CHEND))
                  GO TO 295
C                                       Integer.
 284              WRITE (LINE(CH1:CHEND),1284) RECI(IOFF)
                  GO TO 295
C                                       Logical
 285              IF (RECL(IOFF)) WRITE (LINE(CH1:CHEND),1285)
                  IF (.NOT.RECL(IOFF)) WRITE (LINE(CH1:CHEND),1286)
                  GO TO 295
C                                       Integer (short no longer valid)
 286              IER = 5
                  MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
                  CALL MSGWRT (8)
                  GO TO 999
C
 295           CONTINUE
 300           CONTINUE
 320         CONTINUE
            CALL ZCLC8 (ICRDL, LINE, IC, FITBLK)
 340     CONTINUE
C                                       Last record flush
      IC = ICRDL * ICARD + 1
      IL = 2881 - IC
      IF (IL.LT.2880) THEN
         IF (IL.GT.0) THEN
            LINE (IC:2880) = '                  '
            CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
            END IF
         IL = 2880 / (NBITWD/8)
         CALL COPY (IL, FITBLK, TAPBUF(TBIND))
         CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL TABIO ('CLOS', IRCODE, IRNO, RECI, BUFFER, IERR)
      GO TO 999
C                                       3-D table
 500  IRCODE = 0
C                                       Table control info
      DO 510 II = 1,NCOL
         TPTYPE(II) = IFORMT(II)
         TCOUNT(II) = IFLEN(II)
         TOFF(II) = DATP(II,1)
 510     CONTINUE
C                                       Compress table if possible
      ITNCOL = NCOL
      NEXT = 1
      CALL RFILL (720, 0.0, TAPBUF(TBIND))
C
      DO 740 IRNO = 1,NREC
         CALL TABIO ('READ', IRCODE, IRNO, RECI, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 740
C                                       Use RLINE for scratch output
            DO 700 II = 1,ITNCOL
               ITYPE = TPTYPE(II)
               ILEN = TCOUNT(II)
               IOFF = TOFF(II)
               GO TO (560, 580, 600, 620, 640, 660, 670), ITYPE
C                                       Double precision.
 560              CALL ZRLR64 (ILEN, 1, RECRD(IOFF), DLINE)
                  GO TO 680
C                                       Real.
 580              CALL ZRLR32 (ILEN, 1, RECRR(IOFF), RLINE)
                  GO TO 680
C                                       Character
 600              CALL H2CHR (ILEN, 1, RECH(IOFF), LINE)
                  CALL ZCLC8 (ILEN, LINE, 1, RLINE)
                  GO TO 680
C                                       Integer.
 620              CALL ZILI32 (ILEN, RECI(IOFF), 1, RLINE)
                  GO TO 680
C                                       Logical
 640              DO 650 JJ = 1,ILEN
                     IF (RECL(IOFF+JJ-1)) LINE(JJ:JJ) = 'T'
                     IF (.NOT.RECL(IOFF+JJ-1)) LINE(JJ:JJ) = 'F'
 650                 CONTINUE
                  CALL ZCLC8 (ILEN, LINE, 1, RLINE)
                  GO TO 680
C                                       Integer (short no longer valid)
 660              IER = 5
                  MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
                  CALL MSGWRT (8)
                  GO TO 999
C                                       Bit array
 670              CALL ZXLX8 (ILEN, RECI(IOFF), RLINE)
                  GO TO 680
C                                       Copy to output
 680           NCOPY = NUMBYT(ITYPE) * ILEN
               IF (ITYPE.EQ.7) NCOPY = 1 + (ILEN-1) / (NBITWD/2)
               CALL PTF3D (FDVEC, TBIND, NEXT, RLINE, TAPBUF, NCOPY,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
 700           CONTINUE
 740     CONTINUE
C                                       Last record flush
      IF (NEXT.GT.1) THEN
         CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL TABIO ('CLOS', IRCODE, IRNO, RECI, BUFFER, IERR)
      GO TO 999
C                                       Error on I/O
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
      IER = IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('EXTWRT: WRITING ',I6,' UNFLAGGED ROWS OUT OF ')
 1011 FORMAT (8X,I6,' TOTAL FOR TABLE ',A2,' VERSION ',I3)
 1015 FORMAT ('Writing binary table of type ',A2)
 1040 FORMAT ('BUFFER NOT BIG ENOUGH FOR NEEDED CARD SIZE. BYTES=',I8)
 1050 FORMAT ('XTENSION= ',1H',A8,1H',11X,'/ Extension type')
 1051 FORMAT ('BITPIX  =',20X,'8',1X,'/ Binary data')
 1052 FORMAT ('NAXIS   =',20X,'2',1X,'/ Table is a matrix')
 1053 FORMAT ('NAXIS1  =',13X,I8,1X,'/ Width of table in bytes')
 1054 FORMAT ('NAXIS2  =',13X,I8,1X,'/ Number of entries in table')
 1055 FORMAT ('PCOUNT  =',20X,'0',1X,'/ Random parameter count')
 1056 FORMAT ('GCOUNT  =',20X,'1',1X,'/ Group count')
 1057 FORMAT ('TFIELDS =',19X,I2,1X,'/ Number of fields in each row')
 1058 FORMAT ('EXTNAME = ''AIPS ',A2,1X,'''',11X,'/ AIPS table file')
 1059 FORMAT ('EXTVER  = ',15X,I5,1X,'/ Version number of table')
 1060 FORMAT ('TBCOL',I1,'  = ',15X,I5,1X,'/ Starting Char. pos. of',
     *   ' field',I3)
 1061 FORMAT ('TBCOL',I2 ,' = ',15X,I5,1X,'/ Starting char. pos. of',
     *   ' field',I3)
 1062 FORMAT (I6)
 1070 FORMAT ('TFORM',I1,'  = ''',A8,'''',11X,'/ FORTRAN format',
     *   ' of field',I3)
 1071 FORMAT ('TFORM',I2 ,' = ''',A8,'''',11X,'/ FORTRAN format',
     *   ' of field',I3)
 1072 FORMAT ('TTYPE',I1,'  = ''',A16,'''',3X,'/ Type (heading)',
     *   ' of field',I3)
 1073 FORMAT ('TTYPE',I2,' = ''',A16,'''',3X,'/ Type (heading)',
     *   ' of field',I3)
 1074 FORMAT ('TUNIT',I1,'  = ''',A8,'''',11X,'/ Physical units',
     *   ' of field',I3)
 1075 FORMAT ('TUNIT',I2 ,' = ''',A8,'''',11X,'/ physical units',
     *   ' of field',I3)
 1076 FORMAT (I7,1A1)
 1120 FORMAT (A8,'= ',D25.17)
 1140 FORMAT (A8,'= ',E15.7)
 1160 FORMAT (A8,'= ''',A,'''')
 1180 FORMAT (A8,'= ',I12)
 1200 FORMAT (A8,'=',20X,'T')
 1202 FORMAT (A8,'=',20X,'F')
 1230 FORMAT ('ISORTORD=',I20)
 1240 FORMAT ('END')
 1245 FORMAT ('TABIO RETURNS ERROR',I7,' PROCEED WITH NULL DATA')
 1281 FORMAT (D24.15)
 1282 FORMAT (E15.6)
 1284 FORMAT (I12)
 1285 FORMAT ('T')
 1286 FORMAT ('F')
 1900 FORMAT ('EXTWRT: FITS WRITE ERROR',I7)
      END
