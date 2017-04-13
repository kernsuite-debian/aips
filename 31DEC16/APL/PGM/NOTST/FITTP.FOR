LOCAL INCLUDE 'FITTP.INC'
C                                                          Include FITTP
C                                       Local include for FITTP
      DOUBLE PRECISION HSCAL(2,20), HZERO(2,20), CATSCL, CATOFF
      HOLLERITH XNAME(3), XCLASS(2), XTYPE, XOUTFI(12)
      CHARACTER NAME*12, CLASS*6, TYPE*2, OUTFIL*48, HNAME(20)*8,

     *   LINE*2880
      REAL      DOALL, SEQ4, KVOL4, NTAPE4, DOEOT, TAMROF, BLOCKD,
     *   CNO(2)
      INTEGER   IBVOL, IEVOL, ISEQ, FDVEC(50), TBIND, DLUN, DIND, USER,
     *   ICARD, FITBLK(2880), TABLES, ANTFIL, IFLAG(20), IBLKF, IFMTYP,
     *   KLOCWT, NUMCOR, CATSAV(256), ICNO(2)
      LOGICAL   DODISK, ISCMP, DOBASL
      COMMON /INPARM/ DOALL, XNAME, XCLASS, SEQ4, KVOL4, XTYPE, NTAPE4,
     *   XOUTFI, DOEOT, TAMROF, BLOCKD, CNO
      COMMON /MORPRM/ HSCAL, HZERO, CATSCL, CATOFF, FITBLK, DODISK,
     *   ISCMP, IBVOL, IEVOL, ISEQ, FDVEC, TBIND, DLUN, DIND, USER,
     *   ICARD, TABLES, ANTFIL, IFLAG, IBLKF, IFMTYP, KLOCWT, NUMCOR,
     *   CATSAV, ICNO, DOBASL
      COMMON /FTPCHR/ NAME, CLASS, TYPE, OUTFIL, HNAME, LINE
C                                                          End FITTP.
LOCAL END
LOCAL INCLUDE 'FITTP2.INC'
C                                       Local include for buffers
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   TAPBUF(29184), IBUFSZ
      REAL      BUFF(UVBFSS), TBUFF(UVBFSS)
      COMMON /BUFRS/ TAPBUF, BUFF, TBUFF, IBUFSZ
LOCAL END
      PROGRAM FITTP
C-----------------------------------------------------------------------
C! Translate AIPS data file to a FITS format file
C# Tape Map UV EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002-2005, 2007-2008, 2010-2013, 2015-2016
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
C   This program will write a series of maps or UV data bases
C   using the FITS format.  The antenna file or clean components file
C   will be written as a table of one type or another.
C   Inputs:
C     DOALL      R      True (.GT.0) means do every data base that
C                       matches the inputs.  False (.LE.0) means do
C                       the first data base that matches.
C     INNAME     R(3)   Name of input file.
C     INCLASS    R(2)   Class of file.
C     INSEQ      R      sequence number of file.
C     INDISK     R      disk volume no. of file.
C     INTYPE     R      file type (UV or MA or blank).
C     OUTTAPE    R      tape number for output.
C     OUTFILE(12)R      Disk file name
C     OUTDISK    R      Output disk number
C     DOEOT      R      >= 0 -> advance to end-of-tape before writing
C     FORMAT     R      format types
C                           1 = 16-bit I, 2 32-bit I, 3 = 32-bit IEEE
C     BLOCKING   R      blocking factor 1 - 10
C     CATNO      R(2)   range of catalog numbers
C-----------------------------------------------------------------------
      CHARACTER  CHTM*2
      INTEGER   ISCR(256), IMAX, IERR, IVOL, INDEX, JERR, LSEQ, IRET, I,
     *   JCNO
      LOGICAL   FIND1, T, F, EOF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (ISCR, BUFF)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set No. of files & no. of
C                                       scratch files to zero
      NCFILE = 0
      NSCR = 0
C                                       Get input parameters. Fill in
C                                       defaults.
      CALL FITINI (RQUICK, ISCR, IERR)
      IRET = IERR
      IF (IERR.NE.0) GO TO 990
C                                       Loop for all disks.
      FIND1 = F
      IMAX = 1000000
      IF (DOALL.LE.0.0) IMAX = 1
      IF (ISEQ.LE.0) ISEQ = 0
      LSEQ = ISEQ
      IF ((ISEQ.LE.0) .AND. (DOALL.LE.0.0)) LSEQ = -1
      ICNO(1) = CNO(1) + 0.01
      ICNO(2) = CNO(2) + 0.01
      IF (ICNO(1).LE.0) ICNO(1) = 1
      IF (ICNO(2).LT.ICNO(1)) ICNO(2) = 1000000
      DO 100 IVOL = IBVOL,IEVOL
C                                       Loop for all files on disk.
         JCNO = ICNO(1) - 1
         DO 80 INDEX = 1,IMAX
            JCNO = JCNO + 1
C                                       Find next data base.
            CALL NXTMAP ('READ', IVOL, NAME, CLASS, LSEQ, TYPE, USER,
     *         DLUN, DIND, JCNO, CATBLK, ISCR, EOF, IERR)
            CALL COPY (256, CATBLK, CATSAV)
            CATSCL = 1.0D0
            CATOFF = 0.0D0
            IF (IERR.NE.0) GO TO 900
            IF (EOF) GO TO 90
            IF (JCNO.LE.ICNO(2)) THEN
               FIND1 = T
C                                       Set DIE values for this file.
               NCFILE = 1
               FVOL(1) = IVOL
               FCNO(1) = JCNO
               FRW(1) = 0
C                                       Determine if UV or Map.
               CALL H2CHR (2, KHPTYO, CATH(KHPTY), CHTM)
               IF (CHTM.EQ.'UV') THEN
                  CALL FITSUV (JCNO, IVOL, IERR)
               ELSE
                  CALL FITSMP (JCNO, IVOL, IERR)
                  END IF
               END IF
            CALL MAPCLS ('READ', IVOL, JCNO, DLUN, DIND, CATBLK, F,
     *         ISCR, JERR)
            IF (IERR.NE.0) GO TO 990
            NCFILE = 0
            IF (JCNO.GE.ICNO(2)) GO TO 90
 80         CONTINUE
 90      IF ((IMAX.EQ.1) .AND. (FIND1)) GO TO 900
 100     CONTINUE
 900  IRET = IERR
C                                       Close output
      IF (FIND1) THEN
         I = 1
C                                       BAKF tape to where it already is
C                                       to encourage the OS to not write
C                                       an extra EOF on the tape
         IF (.NOT.DODISK) CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), I,
     *      IERR)
C                                       Then close it
         CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
         IF (IRET.EQ.0) IRET = IERR
C                                       Could not find file of type ...
      ELSE
         IF ((ICNO(1).EQ.1) .AND. (ICNO(2).GE.1000000)) THEN
            MSGTXT = 'CANNOT FIND THE INPUT DATA TO WRITE OUT'
         ELSE
            WRITE (MSGTXT,1900) ICNO
            END IF
         CALL MSGWRT (7)
         WRITE (MSGTXT,1901) USER, NAME, CLASS, ISEQ, IBVOL, IEVOL
         CALL MSGWRT (7)
         IF (IRET.EQ.0) IRET = 1
         CALL ZTPCLS (FDVEC(1), FDVEC(40), IERR)
         END IF
C
 990  CALL DIE (IRET, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1900 FORMAT ('INPUT DATA NOT FOUND IN CATALOG NUMBER RANGE',2I6)
 1901 FORMAT ('LOOKED FOR USER:',I5,1X,A12,'.',A6,'.',I4,' VOLS:',I2,
     *   ' -',I2)
      END
      SUBROUTINE FITINI (RQUICK, ISCR, IRET)
C-----------------------------------------------------------------------
C   FITINI does the most basic inits for FITTP.  Get the parameters,
C   restart AIPS if required, fill in defaults, init the tape I/O.
C   Outputs: RQUICK  L         T -> AIPS already restarted
C            ISCR    I(256)    Scratch buffer
C            IRET    I         Return code : 0 => ok
C                                       else quit
C            COMMON /INPARM/
C            COMMON /MORPRM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, FILUPK*48
      INTEGER   IRET, ISCR(256)
      INTEGER   NPARM, ANP, NP, IERR, NTAPE
      LOGICAL   RQUICK, T, EQUAL
      HOLLERITH HFDVEC(50)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (HFDVEC, FDVEC)
      DATA PRGNAM, NPARM /'FITTP ', 27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Zero fill FDVEC
      CALL FILL (50, 0, FDVEC)
C                                       Init the FITS I/O
      TBIND = 1
C                                       Initialize for AIPS
      CALL GTPARM (PRGNAM, NPARM, RQUICK, DOALL, ISCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
C                                       Only interactive tasks
C                                       Check if disk file output
      ELSE
         CALL H2CHR (48, 1, XOUTFI, FILUPK)
         EQUAL = FILUPK(1:20).EQ.'                    '
         DODISK = .NOT.EQUAL
         IF (((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) .AND.
     *      (.NOT.DODISK)) THEN
            WRITE (MSGTXT,1010)
            CALL MSGWRT (8)
            IRET = 4
            END IF
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Convert characters
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (2, 1, XTYPE, TYPE)
      CALL H2CHR (48, 1, XOUTFI, OUTFIL)
C                                       Disk output.
      IF (DODISK) THEN
         WRITE (MSGTXT,1019) FILUPK
         CALL MSGWRT (4)
C                                       Don't do DOALL
         DOALL = -1.0
         CALL CHR2H (48, OUTFIL, 1, HFDVEC(7))
         FDVEC(1) = 25
         NTAPE = 1
C                                       Tape output
      ELSE
         NTAPE = NTAPE4 + 0.5
         IF (NTAPE.EQ.0) NTAPE = 1
         WRITE (MSGTXT,1030) NTAPE
         CALL MSGWRT (4)
         FDVEC(1) = 129 - NTAPE
         END IF
C                                       Open output file
      FDVEC(5) = NTAPE
      FDVEC(2) = 2880
      FDVEC(3) = (29184 * NBITWD) / 8
      IFMTYP = ABS(TAMROF) + 0.5
C                                        Default to IEEE
      IF ((IFMTYP.LT.1) .OR. (IFMTYP.GT.4)) IFMTYP = 3
      IBLKF = ABS(BLOCKD) + 0.1
      IF (IBLKF.LT.1) IBLKF = 10
      IF (IBLKF.GT.10) IBLKF = 10
      FDVEC(6) = IBLKF
      CALL TAPIO ('OPWT', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Advance to end-of-tape?
      IF (.NOT.DODISK) THEN
         IF (DOEOT.GT.0.0) THEN
            MSGTXT = 'Advancing tape to end-of-information'
            CALL MSGWRT (3)
            NP = 0
            CALL ZTAPE ('AEOI', FDVEC(1), FDVEC(40), NP, IERR)
            IF (IERR.NE.0) GO TO 975
            ANP = ABS (NP) - 1
            IF (NP.GT.0) WRITE (MSGTXT,1060) ANP
            IF (NP.LT.0) WRITE (MSGTXT,1061) ANP
            IF (NP.NE.0) CALL MSGWRT (3)
C                                       Make EOFs and correct position
         ELSE
            MSGTXT = 'Writing at current tape position'
            CALL MSGWRT (3)
            MSGTXT = 'Writing beginning-of-write EOFs'
            CALL MSGWRT (2)
            NP = 1
            CALL ZTAPE ('BEGW', FDVEC(1), FDVEC(40), NP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               GO TO 970
               END IF
            END IF
         END IF
C                                       Set some default values,
C                                       and global parameters.
      USER = NLUSER
      DLUN = 16
      ISEQ = SEQ4 + .5
      IBVOL = KVOL4 + .5
      IEVOL = IBVOL
      IF (IBVOL.EQ.0) THEN
         IBVOL = 1
         IEVOL = NVOL
         END IF
      GO TO 999
C
 970  CALL MSGWRT (8)
 975  CALL ZTPCLS (FDVEC(1), FDVEC(40), IERR)
      GO TO 995
C
 990  CALL MSGWRT (8)
 995  IRET = 16
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET PARMS.  IER=',I3)
 1010 FORMAT ('TAPES NOT AVAILABLE TO BATCH AIPS  ')
 1019 FORMAT ('Writing to disk file: ',A)
 1030 FORMAT ('Writing to tape drive number ',I3)
 1050 FORMAT ('COULD NOT OPEN OUTPUT FILE.  IER=',I4)
 1060 FORMAT ('Advanced to end-of-information after file',I5)
 1061 FORMAT ('Advanced to end-of-information after skipping',I6,
     *   ' files')
 1065 FORMAT ('ERROR',I6,' BEGINNING WRITES WITH EOFS')
      END
      SUBROUTINE FITSMP (ISLOT, KVOL, IERR)
C-----------------------------------------------------------------------
C   FITSMP calls the specific routines for translating an AIPS MAp
C   image to FITS format.
C   INPUTS:
C      ISLOT        I       Catalog slot number of map.
C      KVOL         I       Disk volume number.
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
      INTEGER   KVOL, ISLOT, IERR
C
      INTEGER   I
      CHARACTER CHTMP*18
      REAL      NBLC(7), NTRC(7)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IFMTYP = ABS(TAMROF) + 0.5
      IF ((IFMTYP.LT.1) .OR. (IFMTYP.GT.4)) IFMTYP = 3
C                                       Tell user
      WRITE (MSGTXT,1000) USER, KVOL
      CALL H2CHR (18, 1, CATH(KHIMN), CHTMP)
      CALL NAMEST (CHTMP, CATBLK(KIIMS), MSGTXT(40:80), I)
      CALL MSGWRT (3)
      DO 5 I = 1,7
         NBLC(I) = 0.
         NTRC(I) = 0.
 5       CONTINUE
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), NBLC, NTRC, IERR)
C                                       Convert standard header to FITS
      CALL FITHCN (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'BASIC FITS HEADER', IERR
         GO TO 960
         END IF
C                                       Copy header extra keywords
      CALL FITKEY (ISLOT, KVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'HEADER KEYWORDS', IERR
         GO TO 960
         END IF
C                                       Copy HI file, add AIPS HI
      CALL FTMAHI (ISLOT, KVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'HISTORY', IERR
         GO TO 960
         END IF
C                                       Write mapdata onto tape
      CALL FITDCN (NBLC, NTRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'BINARY IMAGE DATA', IERR
         GO TO 960
         END IF
C                                       Write ext files to tape
      CALL FITEXT (KVOL, ISLOT, IERR)
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
      IF (.NOT.DODISK) CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), I, IERR)
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
C      IERR   I          Error return: 0--> okay
C                        else  from IO routines
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER CHEXTN*8, TCOM*32, CHBLOK*8, BLKCOM*32, BCOM*32
      INTEGER   I, NWORDS, LBPX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
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
C                                       Save old header, insert new
      CATSCL = 1.0D0
      CATOFF = 0.0D0
      IF (IFMTYP.LT.3) THEN
         CATSCL = (CATR(KRDMX) - CATR(KRDMN)) / 65520.0D0
         CATOFF = (CATR(KRDMX) + CATR(KRDMN)) / 2.0D0
         IF (IFMTYP.EQ.2) CATSCL = CATSCL / 65520.0D0
      ELSE IF (IFMTYP.EQ.4) THEN
         CATSCL = (CATR(KRDMX) - CATR(KRDMN)) / 255.D0
         CATOFF = CATR(KRDMN)
         END IF
C                                       Required keywords
      CALL KEYWRD (RWORD(1), RTYPE(1), RPOINT(1), BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       header no longer has bits/pix
      LBPX = 16 * IFMTYP
      IF (IFMTYP.EQ.3) LBPX = -32
      IF (IFMTYP.EQ.4) LBPX = 8
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
      SUBROUTINE FITKEY (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   FITKEY writes history records into FITS headers containing all of
C   the special keywords from the catalog header.  So that readers will
C   recognize them they go out in the form:
C   HISTORY AIPS HEADERi keyword = value / comment
C   where i is the keyword type code (1-5 => double precision, single
C   precision, character*8, integer, logical).
C   Inputs:
C      ISLOT   I      Map slot number
C      IVOL    I      Map disk number
C   Output:
C      IERR    I      Error code from IO routines.
C-----------------------------------------------------------------------
      INTEGER   IVOL, ISLOT, IERR
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
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (DTEMP, RTEMP, LTEMP, ITEMP)
      DATA HCOM /'AIPS Catalog Header Keyword'/
      DATA HKEY /'HISTORY AIPS HEADER'/
C-----------------------------------------------------------------------
C                                       read all header keywords
      NUMKEY = MXKEYS
      CALL CATKEY ('ALL ', IVOL, ISLOT, KEYWOR, NUMKEY, LOCS,
     *   VALUES, KEYTS, BUFF, IERR)
C                                       If any keywords read
      IF (NUMKEY.GT.0) THEN
         DO 20 IKEY = 1,NUMKEY
C                                       Double precsion keyword
            IF (KEYTS(IKEY).EQ.1) THEN
               CALL RCOPY (NWDPDP, VALUES(LOCS(IKEY)), RTEMP)
               WRITE (CARD,1010) HKEY, KEYTS(IKEY), KEYWOR(IKEY), DTEMP,
     *            HCOM
C                                       Single precsion keyword
            ELSE IF (KEYTS(IKEY).EQ.2) THEN
               WRITE (CARD,1011) HKEY, KEYTS(IKEY), KEYWOR(IKEY),
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
C1010 FORMAT (A,I1,2X,A8,' =',1PD19.11,' /',A)
 1010 FORMAT (A,I1,2X,A8,' =',1PE19.11,' /',A)
 1011 FORMAT (A,I1,2X,A8,' =',1PE17.9,' /',A)
 1030 FORMAT (A,I1,2X,A8,' = ''',A,'''',6X,' /',A)
 1040 FORMAT (A,I1,2X,A8,' =',I12,5X,' /',A)
 1050 FORMAT (A,I1,2X,A8,' = ',A1,15X,' /',A)
      END
      SUBROUTINE FTMAHI (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   FTMAHI writes history records on map FITS headers by calling FITHIS
C   to copy the HI file and by adding special records re Clean.
C   Inputs:
C      ISLOT   I      Map slot number
C      IVOL    I      Map disk number
C   Output:
C      IERR    I      Error code from IO routines.
C-----------------------------------------------------------------------
      INTEGER   IVOL, ISLOT, IERR
C
      CHARACTER PRODS(5)*12
      INTEGER   I, J, IC, IL
      REAL      X, Y, Z
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRODS /'NORMAL      ', 'COMPONENTS  ',
     *   'RESIDUAL    ', 'POINTS      ', 'DIRTY MAP   '/
C-----------------------------------------------------------------------
C                                       Copy HI file + general AIPS
      CALL FITHIS (ISLOT, IVOL, IERR)
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
C   FITDCN reads the standard map image data and writes the image on
C   tape in the 16-bit, binary FITS format.
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
      INTEGER   BUFFII(MAXIMG), FITBII(2880)
      REAL      BUFFRR(MAXIMG), FITBRR(2880)
      DOUBLE PRECISION BSC, BZE, NONZER, NZERO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFFII(1), BUFFRR(1))
      EQUIVALENCE (FITBLK(1), FITBII(1), FITBRR(1))
C-----------------------------------------------------------------------
C                                       Initialize
      NONZER = 0.0D0
      NZERO = 0.0D0
      IER = 0
      NBYB = UVBFSS * 2
      BSC = 1.0D0
      BZE = 0.0D0
      IF (CATSCL.GT.1.0E-30) THEN
         BSC = 1.0D0 / CATSCL
         BZE =  -CATOFF / CATSCL
         END IF
      NVAL = 720
      IF (IFMTYP.EQ.1) NVAL = 1440
      IF (IFMTYP.EQ.4) NVAL = 2880
      CALL FILL (2880, 0, FITBLK)
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
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
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
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         CALL MINIT ('READ', DLUN, DIND, INX, INY, IWIN, BUFF, NBYB,
     *      NBKOF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            GO TO 980
            END IF
C                                       Begin read/write loop
         DO 90 I = 1,NY
C                                       Read a map line
            CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR,I
               GO TO 980
               END IF
            CALL FITSNC (NX, IFMTYP, BSC, BZE, BUFF(ININD), BUFFII,
     *         BUFFRR, NONZER, NZERO)
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
               ELSE
                  GO TO 55
                  END IF
               END IF
C                                       Do copy
            IF (IFMTYP.NE.3) THEN
               CALL COPY (NXX, BUFFII(IOFF), FITBLK(1+IBL))
            ELSE
               CALL RCOPY (NXX, BUFFRR(IOFF), FITBRR(1+IBL))
               END IF
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
      SUBROUTINE FITSNC (N, OTYPE, BSC, BZE, RIB, II, RR, NONZER,
     *   NZERO)
C-----------------------------------------------------------------------
C   FITSNC applies any scaling and offsets reqested.
C   Inputs:
C      N        I      Number of pixels
C      OTYPE    I      Type output buffer (I,   I,   R   1-3)
C      BSC      D      Multiply buffer by this
C      BZE      D      Then add this
C      RIB      R(N)   Floating in buffer
C   In/out:
C      NONZER   D      Count of non-zero outputs
C      NZERO    D      Count of 0 outputs
C   Out:
C      II       I(N)   Integer*2 buffer out (OTYPE=1)
C      RR       R(N)   Real*4 buffer out (OTYPE=3)
C-----------------------------------------------------------------------
      INTEGER   N, OTYPE, II(*)
      DOUBLE PRECISION BSC, BZE, NONZER, NZERO
      REAL      RIB(*), RR(*)
C
      DOUBLE PRECISION DTEMP
      INTEGER   MAGIC4, MAGIC1, I, MAGIC, IROUND
      REAL      TEMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      MAGIC1 = 0
      MAGIC = -32768
      MAGIC4 = -2147483647 - 1
C                                       16 bit integer out
      IF (OTYPE.EQ.1) THEN
         DO 10 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               TEMP = RIB(I) * BSC + BZE
               II(I) = IROUND (TEMP)
               IF (II(I).EQ.0) THEN
                  NZERO = NZERO + 1.0D0
               ELSE
                  NONZER = NONZER + 1.0D0
                  END IF
            ELSE
               II(I) = MAGIC
               END IF
 10         CONTINUE
C                                       32 bit integer out
      ELSE IF (OTYPE.EQ.2) THEN
         DO 20 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               DTEMP = RIB(I) * BSC + BZE
               IF (DTEMP.GE.0.0D0) II(I) = DTEMP + 0.5D0
               IF (DTEMP.LT.0.0D0) II(I) = DTEMP - 0.5D0
               IF (II(I).EQ.0) THEN
                  NZERO = NZERO + 1.0D0
               ELSE
                  NONZER = NONZER + 1.0D0
                  END IF
            ELSE
               II(I) = MAGIC4
               END IF
 20         CONTINUE
C                                       Floating output
      ELSE IF (OTYPE.EQ.3) THEN
         DO 30 I = 1,N
            RR(I) = RIB(I)
 30         CONTINUE
C                                       32 bit integer out
      ELSE IF (OTYPE.EQ.4) THEN
         DO 40 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               DTEMP = RIB(I) * BSC + BZE
               IF (DTEMP.GE.0.0D0) II(I) = DTEMP + 0.5D0
               IF (DTEMP.LT.0.0D0) II(I) = DTEMP - 0.5D0
               II(I) = MAX (0, MIN (255, II(I)))
               IF (II(I).EQ.0) THEN
                  NZERO = NZERO + 1.0D0
               ELSE
                  NONZER = NONZER + 1.0D0
                  END IF
            ELSE
               II(I) = MAGIC1
               END IF
 40         CONTINUE
         END IF
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FITSUV (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   This program will write a given UV data base to tape using the
C   FITS format.
C   Inputs:
C      ISLOT  I   Catalog slot number of UV data base.
C      IVOL   I   Disk volume no. of UV data base.
C      COMMON /INPARMS/
C   Outputs:
C      IERR    I   0=ok, 1=fatal error.
C-----------------------------------------------------------------------
      CHARACTER CHTMP*18
      INTEGER   ISLOT, IVOL, IERR, I, JERR
      DOUBLE PRECISION    SCVIS, SCRP(2,20), SCTIM1, SCTIM2, SCWT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IBUFSZ = 2 * UVBFSS
      IFMTYP = ABS(TAMROF) + 0.5
      IF ((IFMTYP.LT.1) .OR. (IFMTYP.GT.4)) IFMTYP = 3
      IF (IFMTYP.NE.3) THEN
         IFMTYP = 3
         MSGTXT = 'FORMAT CHANGED TO IEEE - INTEGER INAPPROPRIATE FOR'
     *      // ' UV DATA'
         CALL MSGWRT (7)
         END IF
C                                       Tell user which map
      WRITE (MSGTXT,1000) USER, IVOL
      CALL H2CHR (18, 1, CATH(KHIMN), CHTMP)
      CALL NAMEST (CHTMP, CATBLK(KIIMS), MSGTXT(41:80), I)
      I = I + 40
      CALL MSGWRT (3)
C                                       Calculate scaling factors.
      CALL CALCSC (ISLOT, IVOL, SCTIM1, SCTIM2, SCRP, SCVIS, SCWT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Write header to tape.
      CALL FTUVHE (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1021) IERR
         GO TO 980
         END IF
C                                       Writer extra header keywords
      CALL FITKEY (ISLOT, IVOL, IERR)
C                                       Write history to tape.
      CALL FTUVHI (ISLOT, IVOL, SCWT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 980
         END IF
C                                       Write data to tape.
      MSGTXT = 'Now writing the uv data'
      CALL MSGWRT (3)
C                                       Compressed data
      IF (ISCMP) THEN
         CALL FTUVDC (SCRP, SCVIS, SCWT, IERR)
C                                       Normal data.
      ELSE
         CALL FTUVDA (SCRP, SCVIS, SCWT, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 980
         END IF
C                                       Write extension tables
      CALL FITEXT (IVOL, ISLOT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         END IF
C                                       End tape file
      MSGTXT = 'Writing end-of-file marks'
      CALL MSGWRT (2)
      CALL TAPIO ('FLSH', FDVEC, TAPBUF, TBIND, IERR)
      GO TO 999
C                                       Error after starting Tape write
 980  CALL MSGWRT (8)
      I = 1
      IF (.NOT.DODISK) CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), I, JERR)
      IF (IERR.EQ.0) IERR = JERR
      CALL ZTPCLS (FDVEC(1), FDVEC(40), JERR)
      IF (IERR.EQ.0) IERR = JERR
      FDVEC(40) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Writing UV data: User',I5,' Disk',I2,'  Name')
 1010 FORMAT ('ERROR FINDING UV SCALING:',I7)
 1021 FORMAT ('ERROR WRITING BASIC UV FITS HEADER:',I7)
 1030 FORMAT ('ERROR WRITING UV HISTORY TO FITS HEADER:',I7)
 1040 FORMAT ('ERROR WRITING UV BINARY DATA TO TAPE:',I7)
 1050 FORMAT ('ERROR WRITING ANTENNA TABLES ON TAPE:',I7)
      END
      SUBROUTINE CALCSC (ISLOT, IVOL, SCTIM1, SCTIM2, SCRP, SCVIS, SCWT,
     *   IERR)
C-----------------------------------------------------------------------
C   CALCSC will calculate the scaling factors to use when converting
C   uv data from the internal format to FITS tape format.
C   Inputs:
C      ISLOT   I     catalog number
C      IVOL    I     disk number
C      COMMON /INPARM/
C   Outputs:
C      SCTIM1  D     1st scaling factor to use for DATE.
C      SCTIM2  D     2nd scaling factor to use for DATE.
C      SCRP    D(2,20)   scaling factor random parameters.  The
C                      second index is for representing a random
C                      parameter as a double integer on tape.
C      OFFSCP  D(2,20)   Offset for random parameters.
C      SCVIS   D       scaling factor array values.
C      SCWT    D       scaling factor for complex-weights.
C      IERR    I   error code. 0=ok, 1=bad.
C      COMMON /MAPHDR/
C         CATBLK(KIPCN) add 1 if TIME1 changed to DATE
C         CATR(KRCIC+JLOCS) STOKES axis inc, change from -1 to 1.
C         CATD(KDCRV+JLOCS) STOKES value at ref pix. Change from -1 to 1
C      COMMON /MORPRM/
C         HSCAL  scaling factors for RP in tape header section.  These
C                are different than the ones we use to convert from our
C                internal format to tape values.
C         HZERO  offsets for RP on tape.
C         HNAME  names of RP on tape.
C         NUMCOR The number of correlator values to write to tape.
C-----------------------------------------------------------------------
      INTEGER   ISLOT, IVOL, IERR
      DOUBLE PRECISION SCTIM1, SCTIM2, SCRP(2,20), SCVIS, SCWT
C
      CHARACTER CHTM8*8
      DOUBLE PRECISION JDAY
      REAL      XMAXRP(20), XMAXVS, XMAXWT
      INTEGER   I, IPTR, ICTR, INO, INDEX, NANT, NVER
      LOGICAL   LBLK
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
C                                       Find any "WEIGHT" axis
      ISCMP = CATBLK(KINAX).EQ.1
      CALL AXEFND (8, 'WEIGHT   ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   IERR)
C                                       Find location of parameters in
C                                       header.
      CALL UVPGET (IERR)
C                                       baseline conversion?
      CALL FNDEXT ('AN', CATBLK, NVER)
      DOBASL = (ILOCB.LT.0) .AND. (NVER.LT.90)
      IF ((DOBASL) .AND. (MAXANT.GT.255)) THEN
         NANT = 0
         CALL FNDEXT ('AN', CATBLK, NVER)
         DO 10 I = 1,NVER
            CALL GETANT (IVOL, ISLOT, I, CATBLK, BUFF, IERR)
            IF (IERR.EQ.0) NANT = MAX (NSTNS, NANT)
 10         CONTINUE
         IF (NANT.GT.255) DOBASL = .FALSE.
         END IF
C                                       Complex-3 has to be first axis
C                                       or else things get too
C                                       complicated.
      IF ((IERR.NE.0) .OR. (JLOCC.NE.0) .OR.
     *   (CATBLK(KINAX).GT.3) .OR. (CATBLK(KINAX).LT.1)) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (7)
         IERR = 10
         GO TO 999
         END IF
C                                       Init values
      CALL FNDEXT ('AN', CATBLK, ANTFIL)
      XMAXWT = 1.0
      XMAXVS = 1.0
      DO 15 I = 1,20
         XMAXRP(I) = 0.0
         IF (I.LE.NRPARM) XMAXRP(I) = 1.0
 15      CONTINUE
C                                       Blanking
      CATR(KRBLK) = 0.
      IF (LBLK) CATR(KRBLK) = FBLANK
C                                       Compressed data?
      IF (ISCMP) THEN
         CATBLK(KINAX)= 3
         NUMCOR = LREC - NRPARM
C                                       Remove excess random parms.
         IF ((KLOCWT+2).EQ.CATBLK(KIPCN)) THEN
            CATBLK(KIPCN) = KLOCWT
            NRPARM = CATBLK(KIPCN)
            END IF
         INDEX = KHPTP + 2 * KLOCWT
         CALL CHR2H (8, '        ', 1, CATH(INDEX))
         INDEX = INDEX + 2
         CALL CHR2H (8, '        ', 1, CATH(INDEX))
      ELSE
C                                       Regular data
         NUMCOR = (LREC - NRPARM) / CATBLK(KINAX)
         END IF
C                                       Set scaling factors.
      SCWT = 1.0D0
      SCVIS = 1.0D0
      CATSCL = 1.0D0 / SCVIS
C                                       Calculate scaling factor for
C                                       writing to tape and for header
C                                       values.
      DO 35 I = 1,20
         SCRP(1,I) = 1.0D0
         SCRP(2,I) = 0.D0
         HSCAL(1,I) = 1.0D0 / SCRP(1,I)
         HSCAL(2,I) = 0.0D0
         HZERO(1,I) = 0.0D0
         HZERO(2,I) = 0.0D0
         IFLAG(I) = 1
         HNAME(I) = ' '
 35      CONTINUE
      DO 36 I = 1,KIPTPN
         CALL H2CHR (8, 1, CATH(KHPTP+(I-1)*2), HNAME(I))
 36      CONTINUE
C                                       Set special scaling factors
C                                       that allow for conversion from
C                                       internal format to FITS format.
      IF ((ILOCU.GE.0).AND.(TYPUVD.LE.0)) THEN
         IPTR = ILOCU  +  1
         HSCAL(1,IPTR) = HSCAL(1,IPTR) / FREQ
         HSCAL(2,IPTR) = HSCAL(2,IPTR) / FREQ
         HNAME(IPTR)(3:4) = '--'
         END IF
      IF ((ILOCV.GE.0).AND.(TYPUVD.LE.0)) THEN
         IPTR = ILOCV + 1
         HSCAL(1,IPTR) = HSCAL(1,IPTR) / FREQ
         HSCAL(2,IPTR) = HSCAL(2,IPTR) / FREQ
         HNAME(IPTR)(3:4) = '--'
         END IF
      IF ((ILOCW.GE.0).AND.(TYPUVD.LE.0)) THEN
         IPTR = ILOCW  +  1
         HSCAL(1,IPTR) = HSCAL(1,IPTR) / FREQ
         HSCAL(2,IPTR) = HSCAL(2,IPTR) / FREQ
         HNAME(IPTR)(3:4) = '--'
         END IF
      IF (ILOCB.GE.0) THEN
         IPTR = ILOCB + 1
         HSCAL(1,IPTR) = 1.0D0
         SCRP(1,IPTR) = 1.0D0
         IFLAG(IPTR) = 1
      ELSE IF (ILOCSA.GE.0) THEN
         IPTR = ILOCSA + 1
         HSCAL(1,IPTR) = 1.0D0
         SCRP(1,IPTR) = 1.0D0
         IFLAG(IPTR) = 1
         IF (DOBASL) THEN
            HNAME(IPTR) = 'BASELINE'
            IPTR = ILOCA1 + 1
            IFLAG(IPTR) = 0
            IPTR = ILOCA2 + 1
            IFLAG(IPTR) = 0
            END IF
         END IF
C                                       Convert TIME1 to two value DATE
      IF (ILOCT.EQ.-1) GO TO 999
         IPTR = ILOCT  +  1
         SCTIM1 = 1.0D0
         SCTIM2 = 1.0D0
         IFLAG(IPTR) = 2
         HNAME(IPTR) = 'DATE'
         HSCAL(1,IPTR) = SCTIM1
         CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
         CALL JULDAY (CHTM8, JDAY)
         HZERO(1,IPTR) = JDAY
         HSCAL(2,IPTR) = SCTIM2
         HZERO(2,IPTR) = 0.0D0
         SCRP(1,IPTR) = 1.0D0 / SCTIM1
         SCRP(2,IPTR) = 1.0D0 / SCTIM2
C                                       Count up number of RP we have.
         ICTR = 0
         INO = CATBLK(KIPCN)
         DO 600 I = 1,INO
            ICTR = ICTR + IFLAG(I)
 600        CONTINUE
         CATBLK(KIPCN) = ICTR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATA SET NOT STANDARD ENOUGH FOR ME')
      END
      SUBROUTINE FTUVHE (IERR)
C-----------------------------------------------------------------------
C   FTUVHE causes standard UV FITS header records to be written on tape.
C   Inputs:
C      COMMON /INPARMS/
C      COMMON /MORPRM/
C   Outputs:  (first 4 in Common)
C      ICARD   I   Card number in current tape buffer (FITBLK).
C      FITBLK  I(2880)   FITS header block work area
C      TAPBUF  I(*)      TAPIO buffers
C      TBIND   I   TAPBUF(TBIND) is the start of the current tape
C                  buffer containing the next block of data to be
C                  written to tape.
C      IERR    I   error code. 0=ok, 1=error.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER CTABLS(2)*2, TCOM*32, ZCOM*32, CHEXTN*8, CHBLOK*8,
     *   BLKCOM*32, COMARR(15)*32, CHTM12*12, BCOM*32, TEMP*8, XWORD*8,
     *   TSTLBL(13)*8
      INTEGER   I, NWORDS, NTABLS, J, XXTYPE(3), XPOINT(3), K, II, INO,
     *   IIII, III, IOFF, IPNT, LBPX, NUMCOM, COMPNT(13), COMNDX,
     *   COMLOP, INDEX
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DKEY.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:VKEY.INC'
      DATA NTABLS, CTABLS /2, 'AN','CH'/
      DATA XXTYPE /8, 6, 7/, XPOINT /8000, 4000, 4000/
      DATA TCOM /'This is the antenna file        '/
      DATA ZCOM /'No standard image just group    '/
      DATA CHEXTN /'EXTEND  '/
      DATA CHBLOK, BLKCOM /'BLOCKED ', 'Tape may be blocked        '/
      DATA NUMCOM /13/
      DATA TSTLBL /
     *   'COMPLEX ', 'STOKES  ', 'FREQ    ',
     *   'IF      ', 'RA      ', 'DEC     ',
     *   'UU-L    ', 'VV-L    ', 'WW-L    ',
     *   'TIME1   ', 'BASELINE', 'SOURCE  ',
     *   'FRQSEL  '/
      DATA COMPNT /2,3,5,6,7,8,9,10,11,12,13,14,15/
      DATA COMARR /'                                ',
     *   '1=real,2=imag,3=weight          ',
     *   '1=I, 2=Q, 3=U, 4=V              ',
     *   '-1=RR, -2=LL, -3=RL, -4=LR      ',
     *   'Frequency in Hz.                ',
     *   'Freq. group no. in CH table     ',
     *   'Right Ascension in deg.         ',
     *   'Declination in deg.             ',
     *   'U baseline component in sec.    ',
     *   'V baseline component in sec.    ',
     *   'W baseline component in sec.    ',
     *   'Time of vis. as Julian date     ',
     *   '256*ant1+ant2+(array#-1)/100    ',
     *   'Source id. in SU table          ',
     *   'Frequency id. in FQ table       ' /
C-----------------------------------------------------------------------
      ICARD = 0
      BCOM = ' '
C                                       Encode required keywords.
      CALL KEYWRD (RWORD(1), RTYPE(1), RPOINT(1), BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      LBPX = -32
      CALL KEYWRD (RWORD(2), 10, LBPX, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      NWORDS = 1 + CATBLK(KIDIM)
      CALL KEYWRD (RWORD(3), 10, NWORDS, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL KEYWRD (RWORD(4), 10, 0, ZCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      NWORDS = 3 + CATBLK(KIDIM)
      DO 10 I = 4,NWORDS
         CALL KEYWRD (RWORD(I+1), RTYPE(I), RPOINT(I), BCOM, IERR)
         IF (IERR.NE.0) GO TO 999
 10      CONTINUE
C                                       Determine if ANtenna ext file
C                                       exists.
      TABLES = 0
      IF (NTABLS.GT.0) THEN
         DO 15 I = 1,NTABLS
            CALL FNDEXT (CTABLS(I), CATBLK, J)
            TABLES = TABLES + J
 15         CONTINUE
         END IF
C                                       Always write AN files if extant
      CALL KEYWRD (CHEXTN, 1, 1, TCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       New FITS BLOCKED = T
      CALL KEYWRD (CHBLOK, 1, 1, BLKCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Encode normal keywords.
      NWORDS = NNT - 2
      DO 20 I = 1,NWORDS
         CALL KEYWRD (NWORD(I), NTYPE(I), NPOINT(I), NCOM(I), IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       Encode axis keywords.
      NWORDS = 5 * CATBLK(KIDIM)
      DO 50 I = 1,NWORDS
C                                       Find comment for label
         INDEX = ((I-1) / 5) * 2 + KHCTP
         COMNDX = 1
         IF (MOD(I, 5) .EQ. 1) THEN
            DO 45 COMLOP = 1,6
               CALL H2CHR (8, 1, CATH(INDEX), CHTM12)
               IF (TSTLBL(COMLOP)(1:8).EQ. CHTM12(1:8))
     *            COMNDX = COMPNT(COMLOP)
 45            CONTINUE
C                                       Correct Stokes' label if nec.
            IF ((COMNDX.EQ.3) .AND. (ICOR0.LT.0)) COMNDX = 4
            END IF
         CALL KEYWRD (AWORD(I+5), ATYPE(I), APOINT(I),
     *      COMARR(COMNDX), IERR)
         IF (IERR.NE.0) GO TO 999
 50      CONTINUE
C                                       Encode GROUP, GCOUNT, PCOUNT.
      DO 60 I = 1,3
         CALL KEYWRD (GWORD(I), GTYPE(I), GPOINT(I), BCOM, IERR)
 60      CONTINUE
C                                       Encode random parameter keywrd.
      I = 0
      K = 0
C                                       Loop for all random parms.
      DO 90 II = 1,NRPARM
C                                       Find comment for label
         INDEX = (II - 1) * 2 + KHPTP
         COMNDX = 1
         CALL H2CHR (8, 1, CATH(INDEX), CHTM12)
         DO 62 COMLOP = 7,NUMCOM
            IF (TSTLBL(COMLOP)(1:4).EQ.CHTM12(1:4)) COMNDX =
     *         COMPNT(COMLOP)
 62         CONTINUE
C                                       Projected RA, Dec
         IF (TSTLBL(5)(1:2).EQ.CHTM12(1:2)) COMNDX = COMPNT(5)
         IF (TSTLBL(6)(1:3).EQ.CHTM12(1:3)) COMNDX = COMPNT(6)
C                                       Sometimes we loop twice for
C                                       double integer or not at all
C                                       for ANTENNA1/2
         INO = IFLAG(II)
C                                       count RP even if omitted
         K = K + 1
         DO 80 IIII = 1,INO
            IOFF = 100 * IIII
            I = I + 1
C                                       Loop for NAME, SCALE, OFFSET
            DO 70 III = 1,3
               IPNT = XPOINT(III) + IOFF + K
               IF (I.LT.10) THEN
                  IF (III.EQ.1) WRITE (TEMP,1060) I
                  IF (III.EQ.2) WRITE (TEMP,1062) I
                  IF (III.EQ.3) WRITE (TEMP,1064) I
               ELSE
                  IF (III.EQ.1) WRITE (TEMP,1065) I
                  IF (III.EQ.2) WRITE (TEMP,1066) I
                  IF (III.EQ.3) WRITE (TEMP,1068) I
                  END IF
               XWORD(1:8) = TEMP(1:8)
               CALL KEYWRD (XWORD, XXTYPE(III), IPNT, COMARR(COMNDX),
     *            IERR)
               COMNDX = 1
 70            CONTINUE
 80         CONTINUE
         IF (IERR.NE.0) GO TO 999
 90      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('PTYPE',I1)
 1062 FORMAT ('PSCAL',I1)
 1064 FORMAT ('PZERO',I1)
 1065 FORMAT ('PTYPE',I2)
 1066 FORMAT ('PSCAL',I2)
 1068 FORMAT ('PZERO',I2)
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
C      FITBLK  I(2880)   output work buffer.
C      TAPBUF  I(*)      TAPIO buffers
C   OUTPUT:
C      IERR    I         error code. 0=ok, 1=bad.
C-----------------------------------------------------------------------
      CHARACTER WORD*8, COMENT*32, CARD*80, STR*12
      INTEGER   ITYPE, POINT, IERR
      INTEGER   KPNTR(58), PNTR, POFF, NBYT, ISTART, IC, ITEND, ITRIM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
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
 500        IF (WORD.EQ.'BSCALE') WRITE (CARD,1220) WORD, CATSCL, COMENT
            IF (WORD.EQ.'BZERO') WRITE (CARD,1220) WORD, CATOFF, COMENT
            GO TO 800
C                                       Get scaling factors from
C                                       common MORPRM.
 600        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, HSCAL(POFF,PNTR), COMENT
            GO TO 800
C                                       Get offsets from MORPRM.
 610        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, HZERO(POFF,PNTR), COMENT
            GO TO 800
C                                       Get RP type from MORPRM
 620        PNTR = MOD (POINT, 100)
            WRITE (CARD,1300) WORD, COMENT
            ITEND = NBYT + 12
            CARD(12:11+NBYT) = HNAME(PNTR)(1:NBYT)
            CARD(ITEND:) = ''' '
            GO TO 800
C                                       Blanking
 630        IF (CATR(PNTR+POFF).EQ.0) GO TO 999
C                                       do not write BLANK for f.p.
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
      SUBROUTINE FTUVHI (ISLOT, IVOL, SCWT, IERR)
C-----------------------------------------------------------------------
C   This routine will write the non UV data base specific history by
C   calling FITHIS.  Then the UV data history and the END card are
C   written to the FITS header.
C   INPUTS:
C      ISLOT   I         catalog slot number of the UV file.
C   In/out: (in common)
C      ICARD   I         The last card written in FITBLK  (1:36)
C      TBIND   I         the start of the current I/O buffer.
C      TAPBUF  I(*)      The TAPIO buffers
C      FITBLK  I(*)      The FITS header work buffer
C   Output:
C      IERR    I         error code. 0=ok, 1 = bad
C-----------------------------------------------------------------------
      CHARACTER SRTYPS(3)*4, EXPL(11)*12, CHTM2*2
      DOUBLE PRECISION    SCWT, WTSCAL
      INTEGER   ISLOT, IVOL, IERR
      INTEGER   IC, IL, I, J1, J2, J, NTYPS
      REAL      X, Y, Z
      LOGICAL   EQUAL
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTYPS, SRTYPS /11, 'BTUV','WRPX','YZM '/
      DATA EXPL /'BASELINE NUM', 'TIME (IAT)  ',
     *           'U VIS. COORD', 'V VIS. COORD',
     *           'W VIS. COORD', 'BASELINE LEN',
     *           'BASELINE PA ', 'DESC ABS(U) ',
     *           'DESC ABS(V) ', 'ASCE ABS(U) ',
     *           'ASCE ABS(V) '/
C-----------------------------------------------------------------------
C                                       Beam parameters
      X = CATR(KRBMJ)
      Y = CATR(KRBMN)
      Z = CATR(KRBPA)
      IF ((X.GT.0.) .OR. (Y.GT.0.) .OR. (Z.NE.0.)) THEN
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         WRITE (MSGTXT,1000) X, Y, Z
         CALL FITCHM (MSGTXT, IC, FITBLK)
         END IF
C                                       Explain baseline number
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      WRITE (MSGTXT,1010)
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       Write general history info.
      CALL FITHIS (ISLOT, IVOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write UV specific stuff.
C                                       Sort order:
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD  -  79
      WRITE (MSGTXT,1011) CATBLK(KITYP)
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       Explain this mess
      CALL H2CHR (2, 1, CATH(KITYP), CHTM2)
      DO 30 I = 1,2
         DO 10 J = 1,NTYPS
            J1 = MOD (J-1, 4) + 1
            J2 = (J-1) / 4 + 1
            EQUAL = CHTM2(I:I) .EQ. SRTYPS(J2)(J1:J1)
            IF (EQUAL) GO TO 20
 10         CONTINUE
         GO TO 30
 20      IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = ICARD * 80 - 79
         WRITE (MSGTXT,1020) CHTM2(I:I), EXPL(J)
         CALL FITCHM (MSGTXT, IC, FITBLK)
 30      CONTINUE
C                                       Scale factor for weights
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = ICARD * 80 - 79
         WTSCAL = 1.0D0 / SCWT
         WRITE (MSGTXT,1030) WTSCAL
         CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       Put END card in buffer.
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      IC = 80 * ICARD + 1
      MSGTXT = 'END     '
      CALL FITCHM (MSGTXT, IC, FITBLK)
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
 1000 FORMAT ('HISTORY AIPS   BMAJ=',1PE12.4,' BMIN=',1PE12.4,
     *   ' BPA=',0PF7.2)
 1010 FORMAT (9X,'/ Where baseline = 256*ant1 + ant2 + (array#-1)/100')
 1011 FORMAT ('HISTORY AIPS   SORT ORDER = ''',A2,'''')
 1020 FORMAT (14X,'/ Where ',A1,' means ',A)
C1030 FORMAT ('HISTORY AIPS WTSCAL = ', 1PD18.11, 1X,
 1030 FORMAT ('HISTORY AIPS WTSCAL = ', 1PE18.11, 1X,
     *   '/ CMPLX WTS=WTSCAL*(TAPE*BSCALE+BZERO)')
      END
      SUBROUTINE FTUVDA (SCRP, SCVIS, SCWT, IERR)
C-----------------------------------------------------------------------
C  Writes FITS format uv data to the output file.
C   Inputs:
C     SCRP    D(2,20)   scaling factors for random parameters.
C     SCVIS   D   scaling factor array values.
C     SCWT    D   scaling factor for complex.weight.  If this is minus,
C                 it is a secret code indicating that the complex
C                 values are complex 2 not complex 3 (ie no weight).
C   In/out: (in common)
C      FITBLK  I(2880)    Header work buffer
C      TAPBUF  I(*)       TAPIO work buffers
C   Output:
C      IERR    I          Error code, 0 => OK
C                             8 => I/O error.
C-----------------------------------------------------------------------
      INTEGER   IERR
      DOUBLE PRECISION SCRP(2,20), SCVIS, SCWT, TAPVR8
      REAL      TAPVAL, RXCNT, FITBR4(1440)
      INTEGER   VO, BO, BIND, LENBU, NIO, I, J, K, IOFF, IPTR, ITMAX,
     *   ITPOS, NCHAN, NVALS, JJ, NCMPLX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBR4)
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
      NCMPLX = 3
      IF (SCWT.LT.0.0D0) NCMPLX = 2
      NCHAN = CATBLK(KINAX+JLOCF)
      NVALS = NUMCOR
C                                       Initialize buffer pointers
      ITPOS = 0
      ITMAX = 720
      RXCNT = 0.0
C                                       Init disk IO.
      LENBU = 0
      CALL UVINIT ('READ', DLUN, DIND, NVIS, VO, LREC, LENBU, IBUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop thru disk file.
 10      RXCNT = RXCNT + 1.0
         CALL UVDISK ('READ', DLUN, DIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) GO TO 999
         IPTR = BIND
C                                       Check if data finished.
         IF (NIO.LE.0) GO TO 300
            DO 290 J = 1,NIO
C                                       Real*4 output
C                                       Loop through random parms.
               DO 230 I = 1,NRPARM
                  IF (IFLAG(I).GT.0) THEN
                     K = I - 1
                     IF ((DOBASL) .AND. (K.EQ.ILOCSA)) THEN
                        TAPVR8 = 256.0*BUFF(IPTR+ILOCA1) +
     *                     BUFF(IPTR+ILOCA2) + (BUFF(IPTR+ILOCSA)-1.)
     *                     /100.
                     ELSE
                        TAPVR8 = SCRP(1,I) * BUFF(IPTR+K)
                        END IF
                     ITPOS = ITPOS + 1
C                                       Write buffer to tape.
                     IF (ITPOS.GT.ITMAX) THEN
                        CALL WRTAUV (ITMAX, IERR)
                        ITPOS = 1
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     IF (IFLAG(I).EQ.2) THEN
                        FITBR4(ITPOS) = TAPVR8
                        TAPVR8 = BUFF(IPTR+K) - (FITBR4(ITPOS) /
     *                     SCRP(1,I))
                        TAPVR8 =  TAPVR8 * SCRP(2,I)
                        ITPOS = ITPOS + 1
                        IF (ITPOS.GT.ITMAX) THEN
                           CALL WRTAUV (ITMAX, IERR)
                           ITPOS = 1
                           IF (IERR.NE.0) GO TO 999
                           END IF
                        END IF
                     FITBR4(ITPOS) = TAPVR8
                     END IF
 230              CONTINUE
C                                       Group array values.
               IOFF = IPTR + NRPARM - 1
C                                       No conversion
               DO 250 I = 1,NVALS
                  DO 245 JJ = 1,NCMPLX
                     IOFF = IOFF + 1
                     TAPVAL = SCVIS * BUFF(IOFF)
                     ITPOS = ITPOS + 1
                     IF (ITPOS.GT.ITMAX) THEN
                        CALL WRTAUV (ITMAX, IERR)
                        ITPOS = 1
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     FITBR4(ITPOS) = TAPVAL
 245                 CONTINUE
 250              CONTINUE
C                                       Incr address, loop
               IPTR = IPTR + LREC
 290           CONTINUE
            GO TO 10
C                                       Write last buffer.
 300  CALL WRTAUV (ITPOS, IERR)
C
 999  RETURN
      END
      SUBROUTINE FTUVDC (SCRP, SCVIS, SCWT, IERR)
C-----------------------------------------------------------------------
C  Writes FITS format uv data to the output file from compressed data.
C   Inputs:
C     SCRP    D(2,20)   scaling factors for random parameters.
C     SCVIS   D   scaling factor array values.
C     SCWT    D   scaling factor for complex.weight.  If this is minus,
C                 it is a secret code indicating that the complex
C                 values are complex 2 not complex 3 (ie no weight).
C   In/out: (in common)
C      FITBLK  I(2880)    Header work buffer
C      TAPBUF  I(*)       TAPIO work buffers
C   Output:
C      IERR    I          Error code, 0 => OK
C                             8 => I/O error.
C-----------------------------------------------------------------------
      INTEGER   IERR
      DOUBLE PRECISION SCRP(2,20), SCVIS, SCWT, TAPVR8
      REAL      TAPVAL, SCLDAT(3), RXCNT, FITBR4(1440), MAGICR
      INTEGER   VO, BO, BIND, LENBU, NIO, I, J, K, IOFF, IPTR, ITMAX,
     *   ITPOS, NCHAN, NVALS, JJ, KINCS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBR4)
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
C                                       Compiler bug workaround
      MAGICR = FBLANK
      NCHAN = CATBLK(KINAX+JLOCF)
      NVALS = NUMCOR
      KINCS = INCS * 3
C                                       Initialize buffer pointers
      ITPOS = 0
      ITMAX = 720
      RXCNT = 0.0
C                                       Init disk IO.
      LENBU = 0
      SCLDAT(1) = SCVIS
      SCLDAT(2) = SCVIS
      SCLDAT(3) = SCVIS * SCWT
      CALL UVINIT ('READ', DLUN, DIND, NVIS, VO, LREC, LENBU, IBUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop thru disk file.
 10      RXCNT = RXCNT + 1.0
         CALL UVDISK ('READ', DLUN, DIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) GO TO 999
         IPTR = BIND
C                                       Check if data finished.
         IF (NIO.LE.0) GO TO 300
            DO 290 J = 1,NIO
C                                       Decompress data.
C                                       If NRPARM=KLOCWT then there are
C                                       WEIGHT and SCALE parameters at
C                                       the end of the random parameter
C                                       list which are not included in
C                                       NRPARM.
               IF (NRPARM.EQ.KLOCWT) THEN
                  CALL ZUVXPN (NVALS, BUFF(IPTR+NRPARM+2),
     *               BUFF(IPTR+KLOCWT), TBUFF)
               ELSE
                  CALL ZUVXPN (NVALS, BUFF(IPTR+NRPARM),
     *               BUFF(IPTR+KLOCWT), TBUFF)
                  END IF
C                                       Real*4 output
C                                       Loop through random parms.
               DO 230 I = 1,NRPARM
                  IF (IFLAG(I).GT.0) THEN
                     K = I - 1
                     IF ((DOBASL) .AND. (K.EQ.ILOCSA)) THEN
                        TAPVR8 = 256.0*BUFF(IPTR+ILOCA1) +
     *                     BUFF(IPTR+ILOCA2) +
     *                     (BUFF(IPTR+ILOCSA)-1.)/100.
                     ELSE
                        TAPVR8 = SCRP(1,I) * BUFF(IPTR+K)
                        END IF
                     ITPOS = ITPOS + 1
C                                       Write buffer to tape.
                     IF (ITPOS.GT.ITMAX) THEN
                        CALL WRTAUV (ITMAX, IERR)
                        ITPOS = 1
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     IF (IFLAG(I).EQ.2) THEN
                        FITBR4(ITPOS) = TAPVR8
                        TAPVR8 = BUFF(IPTR+K) - (FITBR4(ITPOS) /
     *                     SCRP(1,I))
                        TAPVR8 =  TAPVR8 * SCRP(2,I)
                        ITPOS = ITPOS + 1
                        IF (ITPOS.GT.ITMAX) THEN
                           CALL WRTAUV (ITMAX, IERR)
                           ITPOS = 1
                           IF (IERR.NE.0) GO TO 999
                           END IF
                        END IF
                     FITBR4(ITPOS) = TAPVR8
                     END IF
 230              CONTINUE
C                                       Group array values.
               IOFF = 0
C                                       No conversion
               DO 250 I = 1,NVALS
                  DO 245 JJ = 1,3
                     IOFF = IOFF + 1
                     TAPVAL = TBUFF(IOFF) * SCLDAT(JJ)
                     ITPOS = ITPOS + 1
                     IF (ITPOS.GT.ITMAX) THEN
                        CALL WRTAUV (ITMAX, IERR)
                        ITPOS = 1
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     FITBR4(ITPOS) = TAPVAL
 245                 CONTINUE
 250              CONTINUE
C                                       Incr address, loop
               IPTR = IPTR + LREC
 290           CONTINUE
            GO TO 10
C                                       Write last buffer.
 300  CALL WRTAUV (ITPOS, IERR)
C
 999  RETURN
      END
      SUBROUTINE FITHIS (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   This routine will write the general history information to tape.
C   An END card is NOT written.
C   Inputs:
C      ISLOT   I         Catalog slot number of the cataloged file.
C      IVOL    I         Disk number.
C   IN/OUT:  (in common)
C      ICARD   I         The last card written in FITBLK  (1:36)
C      TBIND    I        TAPBUF(TBIND) is the start of the current I/O
C                        buffer.
C      FITBLK  I(1440)   header buffer
C      TAPBUF  I(*)      TAPIO buffers
C   Output:
C      IERR    I         error code. 0=ok, 1=bad>
C-----------------------------------------------------------------------
      CHARACTER HISTRY*8, CHTMP*72, HILINE*80, COFITS(2)*72
      HOLLERITH IBUFF(256)
      INTEGER   ISLOT, IVOL, IERR
      INTEGER   I, IHLUN, LOC, KT, KD, HPTR, IC, IL, J, ID(3), IER,
     *   ITRIM
      LOGICAL   F
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBUFF, BUFF)
      DATA F /.FALSE./
      DATA IHLUN /29/
      DATA HISTRY /'HISTORY '/
C-----------------------------------------------------------------------
      IERR = 0
      COFITS(1) = '  FITS (Flexible Image Transport System) format is'
     *   // ' defined in ''Astronomy'
      COFITS(2) = '  and Astrophysics'', volume 376, page 359;' //
     *   ' bibcode: 2001A&A...376..359H'
C                                       Is there a history file?
      CALL FNDEXT ('HI', CATBLK, I)
      IF (I.LE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Open HI file
      CALL HIINIT (1)
C                                       add a line
      CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1015) IER
         CALL MSGWRT (8)
         GO TO 100
         END IF
      IF (OUTFIL.NE.' ') THEN
         I = ITRIM (OUTFIL)
         IF (I.LE.30) THEN
            WRITE (HILINE,1000) TSKNAM, OUTFIL(:I)
         ELSE
            WRITE (HILINE,1001) TSKNAM, OUTFIL(:I)
            END IF
      ELSE
         WRITE (HILINE,1002) TSKNAM, FDVEC(5)
         END IF
      CALL HIADD (IHLUN, HILINE, IBUFF, IER)
      CALL HICLOS (IHLUN, .TRUE., IBUFF, IER)
C                                       Add HI file into FITS tape
      CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1015) IER
         CALL MSGWRT (8)
         GO TO 100
         END IF
C                                       Initialize HI parameters
C                                       LOC = HI Physical record
C                                       KT = Total # of Logical rec.
C                                       KD = HI logical record
      LOC = 0
      HPTR = 1
      KT = HITAB(HPTR+2)
C                                       Let user know.
      WRITE (MSGTXT, 1200)
      CALL MSGWRT (3)
C                                       Begin HI record loop
      DO 60 KD = 1,KT
         I = MOD (KD-1, NHILPR) + 1
C                                       Read next HI physical record
         IF (I.EQ.1) THEN
            LOC = LOC + 1
            CALL HIIO ('READ', HPTR, LOC, IBUFF, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1020) IER, LOC
               GO TO 90
               END IF
            END IF
         IL = 5 + NHIWPL * MOD (KD-1, NHILPR)
         CALL SPFIL (IBUFF(IL), 72, J)
         CALL H2CHR (72, 1, IBUFF(IL), CHTMP)
C                                       Not AIPS / FITS history
         IF ((CHTMP.NE.COFITS(1)) .AND. (CHTMP.NE.COFITS(2)) .AND.
     *      (CHTMP(1:5).NE.'AIPS ')) THEN
C                                       Increment output card cnt
C                                       Write TAPBUF to tape.
            IF (ICARD.GE.36) CALL WRCTAP (IERR)
            IF (IERR.NE.0) GO TO 999
            ICARD = ICARD + 1
            IC = 80 * ICARD - 79
C                                       Copy HI data into buffer
            HILINE(1:8) = HISTRY
C                                       See if this card already starts
C                                       with word history.
            IF (CHTMP(1:8).EQ.HISTRY) THEN
C                                       "HISTORY" in card
               HILINE(9:72) = CHTMP(9:72)
               HILINE(73:80) = ' '
            ELSE
               HILINE(9:80) = CHTMP
               END IF
C                                       Card to FITBLK
            CALL ZCLC8 (80, HILINE, IC, FITBLK)
            END IF
 60      CONTINUE
C                                       Close HI file
 90   CALL HICLOS (IHLUN, F, IBUFF, IER)
C                                       extra parameters
C                                       ORIGIN keyword
 100  IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      WRITE (MSGTXT,1100) HSTNAM, SYSNAM, RLSNAM
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       DATE keyword
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      CALL ZGDATE (ID)
      WRITE (CHTMP,1110) ID
      CALL DATFST ('L2F', CHTMP)
      I = ITRIM (CHTMP)
C                                       year 2000 set by DATFST only
      IF (I.LE.8) THEN
         WRITE (MSGTXT,1111) CHTMP(:I)
      ELSE
         WRITE (MSGTXT,1112) CHTMP(:I)
         END IF
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       history: names
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      WRITE (MSGTXT,1120) CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), MSGTXT(24:35))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), MSGTXT(47:52))
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       history: user number
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      WRITE (MSGTXT,1130) CATBLK(KIIMU)
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       history: FITS ref 1
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      MSGTXT = 'COMMENT ' // COFITS(1)
      CALL FITCHM (MSGTXT, IC, FITBLK)
C                                       history: FITS ref 2
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.NE.0) GO TO 999
      ICARD = ICARD + 1
      IC = 80 * ICARD - 79
      MSGTXT = 'COMMENT ' // COFITS(2)
      CALL FITCHM (MSGTXT, IC, FITBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'DATAOUT = ''',A,''' / data written to disk file')
 1001 FORMAT (A6,'DATAOUT = ''',A,''' / disk file')
 1002 FORMAT (A6,'OUTTAPE = ',I2,'   / data written to tape')
 1010 FORMAT ('Warning: No history file to put on FITS output')
 1015 FORMAT ('FITHCN: COULD NOT OPEN HI FILE.  IER=',I4)
 1020 FORMAT ('FITHCN: COULD NOT READ HI FILE.  IER=',I4,
     *   '  RECORD #=',I5)
 1100 FORMAT ('ORIGIN  = ''AIPS',A12,1X,A20,1X,A7,'''',4X,'/ ')
 1110 FORMAT (I4.4,2I2.2)
 1111 FORMAT ('DATE    = ''',A,'''',5X,'/ File written on dd/mm/yy')
 1112 FORMAT ('DATE    = ''',A,'''',3X,
     *   '/ File written on Greenwich yyyy-mm-dd')
 1120 FORMAT ('HISTORY AIPS   IMNAME=''',12X,''' IMCLASS=''',6X,
     *   ''' IMSEQ=',I4,5X,'/ ')
 1130 FORMAT ('HISTORY AIPS   USERNO=',I5,12X,'/ ')
 1200 FORMAT ('Writing HI file')
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
      INTEGER   ITFULL, IERR
C
      INTEGER   ITMAX, ITZ
      REAL      FITBR4(1440)
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBR4)
C-----------------------------------------------------------------------
C                                       Write this buffer to tape.
      ITMAX = 720
      IF (IFMTYP.EQ.1) ITMAX = 1440
      IF (IFMTYP.EQ.4) ITMAX = 2880
      ITZ = ITMAX - ITFULL
      IF (ITZ.GT.0) THEN
         IF (IFMTYP.EQ.3) THEN
            CALL RFILL (ITZ, 0.0, FITBR4(1+ITFULL))
         ELSE
            CALL FILL (ITZ, 0, FITBLK(1+ITFULL))
            END IF
         END IF
C                                       translate
      IF (IFMTYP.EQ.1) CALL ZILI16 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.2) CALL ZILI32 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.3) CALL ZRLR32 (ITMAX, 1, FITBLK, TAPBUF(TBIND))
      IF (IFMTYP.EQ.4) CALL ZILI8 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
C                                       write
      CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
      END
      SUBROUTINE WRTAUV (ITFULL, IERR)
C-----------------------------------------------------------------------
C   Writes TAPBUF to tape and resets counters.
C   for UV data with no magic blanks
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
      INTEGER   ITFULL, IERR
C
      INTEGER   ITMAX, ITZ
      REAL      FITBR4(1440)
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBR4)
C-----------------------------------------------------------------------
C                                       Write this buffer to tape.
      ITMAX = 720
      IF (IFMTYP.EQ.1) ITMAX = 1440
      IF (IFMTYP.EQ.4) ITMAX = 2880
      ITZ = ITMAX - ITFULL
      IF (ITZ.GT.0) THEN
         IF (IFMTYP.EQ.3) THEN
            CALL RFILL (ITZ, 0.0, FITBR4(1+ITFULL))
         ELSE
            CALL FILL (ITZ, 0, FITBLK(1+ITFULL))
            END IF
         END IF
C                                       translate
      IF (IFMTYP.EQ.1) CALL ZILI16 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.2) CALL ZILI32 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      IF (IFMTYP.EQ.3) CALL ZRLRUV (ITMAX, 1, FITBLK, TAPBUF(TBIND))
      IF (IFMTYP.EQ.4) CALL ZILI8 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
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
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
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
      SUBROUTINE FITEXT (KVOL, ISLOT, IER)
C-----------------------------------------------------------------------
C   FITEXT copies AIPS tables files to the FITS output file.  All
C   tables will be written as 3-D tables unless DOASC=.TRUE.; in this
C   case, all files which can be written as ASCII tables will be
C   written thus and others will be written as 3-D tables.
C   Inputs:  KVOL   I     Disk number
C            ISLOT  I     Catalog slot number of map file.
C   Output:  IER    I     Error number: 0 => none
C                           else count of errors or tape IO error #
C-----------------------------------------------------------------------
      INTEGER   KVOL, ISLOT, IER
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TTYPE*2
      INTEGER   NEXTF, ITABLE, I20, MAXVER, IVER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Loop for all extension files.
      CALL FXHDEX (CATBLK)
      NEXTF = KHEXT + KIEXTN - 1
      DO 800 ITABLE = KHEXT, NEXTF
C                                       Get max # Table versions
         I20 = KIVER + ITABLE - KHEXT
         MAXVER = CATBLK(I20)
         IF (MAXVER.GT.0) THEN
            CALL H2CHR (2, 1, CATH(ITABLE), TTYPE)
C                                       Loop over versions
            DO 780 IVER = 1,MAXVER
C                                       Do table
               CALL EXTWRT (TTYPE, IVER, KVOL, ISLOT, IER)
               IF (IER.NE.0) GO TO 999
 780           CONTINUE
            END IF
 800     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EXTWRT (TTYPE, IVER, KVOL, ISLOT, IER)
C-----------------------------------------------------------------------
C   EXTWRT copies a single, specified AIPS table file to the FITS
C   output file.  Table will be written as 3-D tables unless
C   DOASC=.TRUE.; in this case, a file which can be written as an ASCII
C   table will be  written thus else it will be written as a 3-D table.
C   Inputs:  TTYPE  C*2   Extension table type, 2 char.
C            IVER   I     Version number.
C            KVOL   I     Disk number
C            ISLOT  I     Catalog slot number of map file.
C   Output:  IER    I     Error number: 0 => none
C                           else count of errors or tape IO error #
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C                                       XBPRSZ is maximum size of an
C                                       array in a table record in real
C                                       words, worst case is bandpass
C                                       table.
      CHARACTER COLHDR(128)*24, UNITS(128)*8, KWNAME*8, TTYPE*2,
     *   FORM3D*8, FORMXX*8, FORCOD(7)*1, FORM(7)*8, TYPTAB(2)*8,
     *   FRMOUT*8, STARS*32, CHTMP*10
      HOLLERITH RECH(XBPRSZ), HVALUE(2)
      INTEGER   KVOL, ISLOT, IER
      DOUBLE PRECISION RECORD(XBPRSZ/2), DVALUE, RECRD(XBPRSZ/2),
     *   DLINE(XBPRSZ/2)
      REAL   RLINE(XBPRSZ), RECRR(XBPRSZ), RVALUE
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
      INCLUDE 'FITTP.INC'
      INCLUDE 'FITTP2.INC'
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
      CALL ISTAB (TTYPE, KVOL, ISLOT, IVER, LUN, BUFFER,
     *   TABLE, EXIST, FITASC, IERR)
      IF (.NOT.EXIST) GO TO 999
      IF (.NOT.TABLE) GO TO 999
      CALL TABINI ('READ', TTYPE, KVOL, ISLOT, IVER, CATBLK, LUN, NKEY,
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
C                                       Try to get character format
C                                       into the right form.
         IF (II.EQ.3) THEN
            WRITE (MSGTXT,1062) IFLEN(I)
            CALL CHTRIM (MSGTXT, 8, MSGTXT, ILEN)
            FORM(3) = 'A' // MSGTXT(1:ILEN)
            END IF
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
C                                       Double Precision.
            IF (ITYPE.EQ.1) THEN
               WRITE (MSGTXT,1120) KWNAME, DVALUE
C                                       Real.
            ELSE IF (ITYPE.EQ.2) THEN
               RVALUE = DVALUE
               WRITE (MSGTXT,1140) KWNAME, RVALUE
C                                       Character.
            ELSE IF (ITYPE.EQ.3) THEN
               CALL H2CHR (8, 1, HVALUE, CHTMP)
               IF (INDEX(KWNAME,'DATE').GT.0) CALL DATFST ('L2F', CHTMP)
               I = MAX (8, ITRIM (CHTMP))
               WRITE (MSGTXT,1160) KWNAME, CHTMP(:I)
C                                       Integer.
            ELSE IF (ITYPE.EQ.4) THEN
               IF (DVALUE.LT.0.0D0) JVALUE = DVALUE - 0.01D0
               IF (DVALUE.GE.0.0D0) JVALUE = DVALUE + 0.01D0
               WRITE (MSGTXT,1180) KWNAME, JVALUE
C                                       Logical.
            ELSE IF (ITYPE.EQ.5) THEN
               IF (DVALUE.GT.0.0D0) WRITE (MSGTXT,1200) KWNAME
               IF (DVALUE.LE.0.0D0) WRITE (MSGTXT,1202) KWNAME
C                                       Integer (short no longer valid)
C           ELSE IF (ITYPE.EQ.6) THEN
C              IER = 5
C              MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
C              CALL MSGWRT (8)
C              GO TO 999
               END IF
C
            CALL FT2CHM (MSGTXT, ICARD, FITBLK)
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
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1245) IERR
               CALL MSGWRT (6)
               DO 260 II = 1, NCOL
                  ITYPE = IFORMT(II)
                  ILEN = ITLEN(ITYPE) + IFLEN(II)
                  LINE(IBCOL(II):IBCOL(II)+ILEN-1) = STARS(1:ILEN)
 260              CONTINUE
               GO TO 320
               END IF
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
C                                       Double precision.
               IF (ITYPE.EQ.1) THEN
                  WRITE (LINE(CH1:CHEND),1281) RECRD(IOFF)
C                                       Real.
               ELSE IF (ITYPE.EQ.2) THEN
                  WRITE (LINE(CH1:CHEND),1282) RECRR(IOFF)
C                                       Character
               ELSE IF (ITYPE.EQ.3) THEN
                  CALL H2CHR (ILEN, 1, RECH(IOFF), LINE(CH1:CHEND))
C                                       Integer.
               ELSE IF (ITYPE.EQ.4) THEN
                  WRITE (LINE(CH1:CHEND),1284) RECI(IOFF)
C                                       Logical
               ELSE IF (ITYPE.EQ.5) THEN
                  IF (RECL(IOFF)) THEN
                     WRITE (LINE(CH1:CHEND),1285)
                  ELSE
                     WRITE (LINE(CH1:CHEND),1286)
                     END IF
C                                       Integer (short no longer valid)
               ELSE
                  IER = 5
                  MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
 300           CONTINUE
 320        CALL ZCLC8 (ICRDL, LINE, IC, FITBLK)
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
C                                       Double precision.
               IF (ITYPE.EQ.1) THEN
                  CALL ZRLR64 (ILEN, 1, RECRD(IOFF), DLINE)
C                                       Real.
               ELSE IF (ITYPE.EQ.2) THEN
                  CALL ZRLR32 (ILEN, 1, RECRR(IOFF), RLINE)
C                                       Character
               ELSE IF (ITYPE.EQ.3) THEN
                  CALL H2CHR (ILEN, 1, RECH(IOFF), LINE)
                  CALL ZCLC8 (ILEN, LINE, 1, RLINE)
C                                       Integer.
               ELSE IF (ITYPE.EQ.4) THEN
                  CALL ZILI32 (ILEN, RECI(IOFF), 1, RLINE)
C                                       Logical
               ELSE IF (ITYPE.EQ.5) THEN
                  DO 650 JJ = 1,ILEN
                     IF (RECL(IOFF+JJ-1)) LINE(JJ:JJ) = 'T'
                     IF (.NOT.RECL(IOFF+JJ-1)) LINE(JJ:JJ) = 'F'
 650                 CONTINUE
                  CALL ZCLC8 (ILEN, LINE, 1, RLINE)
C                                       Bit array
               ELSE IF (ITYPE.EQ.7) THEN
                  CALL ZXLX8 (ILEN, RECI(IOFF), RLINE)
C                                       Integer (short no longer valid)
               ELSE
                  IER = 5
                  MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
C                                       Copy to output
               NCOPY = NUMBYT(ITYPE) * ILEN
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
      SUBROUTINE FQ2CH (DISK, CNO, DOFQCH, FQTRN, IERR)
C-----------------------------------------------------------------------
C  Routine to determine if an FQ table is present, if it is and it
C  has only one row then it will be translated to a CH table. An
C  exception is if the one row has an FQ ID > 1, in which case the
C  translation must not occur.
C  Input:
C    DISK             I       Vol. number on which FQ table exists
C    CNO              I       Catalogue number
C    DOFQCH           L       If TRUE will try to translate FQ -> CH
C                             if possible
C  Output:
C    FQTRN            L       True if FQ translated to CH
C    IERR             I       Error code, 0=> OK
C-----------------------------------------------------------------------
      INTEGER IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, BNDCOD(MAXIF)*8
      INTEGER DISK, CNO, VER, LUN, BUFFER(512), JERR, IFQRNO,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), NIF, NUMFQE, FQID, FQVER,
     *   FQSID(MAXIF), ISBAND(MAXIF), IDUM, I
      LOGICAL TABLE, FQEXIS, FITASC, CHSTAT, FQTRN, DOFQCH
      REAL    FQCHB(MAXIF), FQTBW(MAXIF)
      DOUBLE PRECISION FQFRQ(MAXIF), FOFF(MAXIF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      IERR = 0
      FQTRN = .FALSE.
      IF (.NOT.DOFQCH) GO TO 999
C                                       Does FQ table exist
      VER = 1
      CALL ISTAB ('FQ', DISK, CNO, VER, LUN, BUFFER, TABLE, FQEXIS,
     *   FITASC, JERR)
      IF (.NOT. FQEXIS) GO TO 999
C                                       Read it and check number
C                                       of rows
      CALL FQINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NIF, IERR)
      IF (IERR.NE.0) GO TO 999
      NUMFQE = BUFFER(5)
      FQVER = VER
      IF (NUMFQE.GT.1) THEN
         CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, JERR)
         GO TO 999
         END IF
C                                       Otherwise read and store
      IFQRNO = 1
      CALL TABFQ ('READ', BUFFER, IFQRNO, FQKOLS, FQNUMV, NIF, FQID,
     *   FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (FQID.NE.1) THEN
         CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, JERR)
         GO TO 999
         END IF
      DO 100 I = 1, NIF
         FOFF(I) = FQFRQ(I)
         ISBAND(I) = FQSID(I)
 100     CONTINUE
      CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, JERR)
C                                       Create CH table
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, BUFFER, IERR)
      IF (STAT.EQ.'READ') THEN
C                                       Change status
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'CLRD'
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'WRIT'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       # rows in old table
      VER = 1
      CALL OLDCHN ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, NIF,
     *   FOFF, ISBAND, IERR)
      FQTRN = .TRUE.
      IF (IERR.NE.0) GO TO 999
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'CLWR'
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ'
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FQ2CH: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
      SUBROUTINE OLDCHN (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, IERR)
C-----------------------------------------------------------------------
C   Creates and fills or reads CH (IF descriptor) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C   Input/Output:
C      VER      I        CH file version
C      NIF      I        Number of IFs.
C      FOFF     D(*)     Frequency offset in Hz from ref. freq.
C                           True = reference + offset.
C      ISBAND   I(*  )   Sideband of each IF.
C                        -1 => 0 video freq. is high freq. end
C                         1 => 0 video freq. is low freq. end
C   Output:
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      CHARACTER CHIF*8, OPCODE*4, TTITLE*56, TITLE(3)*24, UNITS(3)*8
      HOLLERITH HOLTMP(6)
      INTEGER   BUFFER(512), DISK, CNO, VER, LUN, IERR, CATBLK(256),
     *   NKEY, NREC, DATP(128,2), NCOL, CHKOLS(3), NTT, DTYP(3), NDATA,
     *   ISBAND(*), RECI(8), JERR, IFOFF, NOKOL, OFFKOL, SBKOL, NIF,
     *   INTTMP(6)
      LOGICAL   DOREAD
      INTEGER   ICHRNO, NUMCH, I, J
      DOUBLE PRECISION  FOFF(*), RECD(4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INTTMP, HOLTMP)
      EQUIVALENCE (CHKOLS(1), NOKOL), (CHKOLS(2), OFFKOL),
     *   (CHKOLS(3), SBKOL),     (RECI, RECD)
      DATA NTT /56/
      DATA CHIF /'IF      '/
      DATA TTITLE /'AIPS UV DATA FILE IF DESCRIPTOR TABLE           '/
      DATA NDATA /3/
      DATA DTYP /14,11,14/
      DATA TITLE /'IF NO.                  ',
     *   'FREQUENCY OFFSET        ', 'SIDEBAND                '/
      DATA UNITS /'        ', 'HZ      ', '        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       See if CH table should be there
      CALL AXEFND (4, CHIF, CATBLK(KIDIM), CATBLK(KHCTP), IFOFF, JERR)
      IF ((JERR.NE.0) .OR. (IFOFF.LT.0)) GO TO 500
C                                       Open file
      NREC = 20
      NCOL = NDATA
      NKEY = 0
C                                       Fill in types
      CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Create/open file
      CALL TABINI (OPCODE, 'CH', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       See if file exists.
      IF (IERR.NE.0) THEN
C                                       File created, initialize
         DO 40 I = 1,NDATA
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, INTTMP, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, INTTMP, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
 40         CONTINUE
         END IF
C                                       Get number of Channels
      NUMCH = BUFFER(5)
C                                       Set NIF
      IF (DOREAD) NIF = NUMCH
C                                       Fill in Table title
      IF (.NOT.DOREAD) CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Get array indices
      DO 150 I = 1,NDATA
         CHKOLS(I) = DATP(I,1)
 150     CONTINUE
C                                       Read/write table entries
      DO 200 I = 1,NIF
         ICHRNO = I
C                                       If write fill RECORD
         IF (.NOT.DOREAD) THEN
            RECI(NOKOL) = I
            RECI(SBKOL) = ISBAND(I)
            RECD(OFFKOL) = FOFF(I)
            END IF
C                                       Process record.
         CALL TABIO (OPCODE, 0, ICHRNO, RECI, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 999
         IF (IERR.LT.0) GO TO 200
C                                       If READ pick data from RECORD.
         IF (DOREAD) THEN
            J = RECI(NOKOL)
            FOFF(J) = RECD(OFFKOL)
            ISBAND(J) = RECI(SBKOL)
            END IF
 200     CONTINUE
C                                       Close
      CALL TABIO ('CLOS', 0, ICHRNO, RECI, BUFFER, IERR)
      GO TO 999
C                                       Case of no or 1 IF axis.
C                                       If write just return
 500  IF (DOREAD) THEN
         NIF = 1
         FOFF(1) = 0.0D0
         ISBAND(1) = 0
         END IF
C
 999  RETURN
      END
