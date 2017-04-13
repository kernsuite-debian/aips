LOCAL INCLUDE 'FITAB.INC'
C                                                          Include FITAB
C                                       Local include for FITAB
      DOUBLE PRECISION HSCAL(20), HZERO(20), CATSCL, CATOFF, TIMBEG,
     *   TIMEND, TFIRST, TLAST
      HOLLERITH XNAME(3), XCLASS(2), XTYPE, XOUTFI(12)
      CHARACTER NAME*12, CLASS*6, TYPE*2, OUTFIL*48, HNAME(20)*16,
     *   LINE*2880, HUNIT(20)*8
      REAL      DOALL, SEQ4, KVOL4, NTAPE4, DOEOT, TAMROF, BLOCKD,
     *   QUANT, DOUVCM, XPIECE, XDROP, CNO(2)
      INTEGER   IBVOL, IEVOL, ISEQ, FDVEC(50), TBIND, DLUN, DIND, USER,
     *   ICARD, FITBLK(2880), TABLES, IBLKF, IFMTYP, KLOCWT,
     *   NUMCOR, CATSAV(256), FSTVIS, LSTVIS, HPTR(20), NOPARM, NPIECE,
     *   FPIECE, IPIECE, IILOCB
      LOGICAL   DODISK, ISCMP, FIRSTP, BYTIME, ISUV, DOBASL
      COMMON /INPARM/ DOALL, XNAME, XCLASS, SEQ4, KVOL4, XTYPE, NTAPE4,
     *   XOUTFI, DOEOT, TAMROF, BLOCKD, QUANT, DOUVCM, XPIECE, XDROP,
     *   CNO
      COMMON /MORPRM/ HSCAL, HZERO, CATSCL, CATOFF, TIMBEG, TIMEND,
     *   TFIRST, TLAST, CATSAV, FITBLK, DODISK, ISCMP, IBVOL, IEVOL,
     *   ISEQ, FDVEC, TBIND, DLUN, DIND, USER, ICARD, TABLES,
     *   IBLKF, IFMTYP, KLOCWT, NUMCOR, FIRSTP, BYTIME, FSTVIS, LSTVIS,
     *   HPTR, NOPARM, NPIECE, FPIECE, IPIECE, ISUV, DOBASL, IILOCB
      COMMON /FTPCHR/ NAME, CLASS, TYPE, OUTFIL, HNAME, HUNIT, LINE
C                                                          End FITAB.
LOCAL END
LOCAL INCLUDE 'FITAB2.INC'
C                                       Local include for buffers
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   TAPBUF(29184), IBUFSZ
      REAL      BUFF(UVBFSS), TBUFF(UVBFSS)
      COMMON /BUFRS/ TAPBUF, BUFF, TBUFF, IBUFSZ
LOCAL END
      PROGRAM FITAB
C-----------------------------------------------------------------------
C! Translate AIPS data file to a FITS format file
C# Tape Map UV EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2005, 2007-2008, 2010-2013, 2015-2016
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
C     DOUVCM     R      > 0 write as compressed data (1 wt, RE and Im
C                       16-bit integer, else use 32-bit float
C     NPIECE     R      Number of pieces to break uv data into
C     BDROP      R      But don't write the 1st BDROP pieces to tape
C-----------------------------------------------------------------------
      CHARACTER  CHTM*2
      INTEGER   ISCR(256), JCNO, IMAX, IERR, IVOL, INDEX, JERR, LSEQ,
     *   IRET, I, ICNO(2)
      LOGICAL   FIND1, T, F, EOF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
               ISUV = CHTM.EQ.'UV'
               IF (ISUV) THEN
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
C   FITINI does the most basic inits for FITAB.  Get the parameters,
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
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (HFDVEC, FDVEC)
      DATA PRGNAM, NPARM /'FITAB ', 31/
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
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 20
C                                       Only interactive tasks
C                                       Check if disk file output
 10   CALL H2CHR (48, 1, XOUTFI, FILUPK)
      EQUAL = FILUPK(1:20).EQ.'                    '
      DODISK = .NOT.EQUAL
      IF (((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) .OR. (DODISK))
     *   GO TO 20
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         IRET = 4
C                                       Restart AIPS
 20   IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
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
C                                       Advance to end-of-tape?
      IF (.NOT.DODISK) THEN
         CALL TAPIO ('OPWT', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
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
      DLUN = 26
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
      INTEGER   I, IX, IROUND
      CHARACTER CHTMP*18
      REAL      NBLC(7), NTRC(7), RX, RN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      NPIECE = 0
      IFMTYP = ABS(TAMROF) + 0.5
      IF ((IFMTYP.LT.1) .OR. (IFMTYP.GT.4)) IFMTYP = 3
C                                       Tell user
      WRITE (MSGTXT,1000) USER, KVOL
      CALL H2CHR (18, 1, CATH(KHIMN), CHTMP)
      CALL NAMEST (CHTMP, CATBLK(KIIMS), MSGTXT(40:80), I)
      CALL MSGWRT (3)
C                                       quantization
      IF ((QUANT.GT.0.0) .AND. (IFMTYP.EQ.4)) THEN
         MSGTXT = 'QUANTIZATION TURNED OFF FOR 8-BIT INTEGER IMAGES'
         CALL MSGWRT (7)
         QUANT = 0.0
         END IF
      IF (QUANT.GT.0.0) THEN
         RX = ABS (CATR(KRDMX)) / QUANT
         RN = ABS (CATR(KRDMN)) / QUANT
         RX = MAX (RX, RN)
         IF (RX.LT.32765.0) THEN
            IF (IFMTYP.EQ.2) IFMTYP = 1
         ELSE
            IF (IFMTYP.EQ.1) IFMTYP = 2
            IF ((RX.GT.2.0D0**31-2.0D0) .AND. (IFMTYP.NE.3)) THEN
               MSGTXT = 'QUANTIZATION TOO FINE FOR I*4 INTEGERS'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 999
               END IF
            END IF
         IX = IROUND (RX)
         WRITE (MSGTXT,1001) QUANT, IX
         CALL MSGWRT (4)
         END IF

      DO 5 I = 1,7
         NBLC(I) = 0.
         NTRC(I) = 0.
 5       CONTINUE
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), NBLC, NTRC, IERR)
C                                       open disk file
      IF (.NOT.DODISK) THEN
         CALL TAPIO ('OPWT', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            GO TO 990
            END IF
         END IF
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
         WRITE (MSGTXT,1010) 'HISTORY ', IERR
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
 990  IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Writing image: User',I5,'  Disk',I2,'  Name')
 1001 FORMAT ('Due to quantization',1PE10.3,' dynamic range is',I11,
     *   ' to 1')
 1005 FORMAT ('COULD NOT OPEN OUTPUT FILE.  IER=',I4)
 1010 FORMAT ('ERROR WRITING ',A,':',I7)
      END
      SUBROUTINE FITHCN (IERR)
C-----------------------------------------------------------------------
C   FITHCN converts the standard map header into the FITS format and
C   writes this header on tape.
C   Inputs:
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
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
         IF (QUANT.GT.0.0) THEN
            CATSCL = QUANT
         ELSE
            CATSCL = (CATR(KRDMX) - CATR(KRDMN)) / 65520.0D0
            CATOFF = (CATR(KRDMX) + CATR(KRDMN)) / 2.0D0
            IF (IFMTYP.EQ.2) CATSCL = CATSCL / 65520.0D0
            END IF
      ELSE IF (IFMTYP.EQ.4) THEN
         CATSCL = (CATR(KRDMX) - CATR(KRDMN)) / 255.0D0
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
      INTEGER   LOCS(MXKEYS), KEYTS(MXKEYS), ITEMP, IKEY, NUMKEY, I,
     *   ITRIM
      CHARACTER KEYWOR(MXKEYS)*8, HCOM*27, CARD*80, HKEY*19, LOGKEY*1,
     *   DATEST*12
      LOGICAL   LTEMP
      REAL      VALUES(MXKEYS), RTEMP(2)
      DOUBLE PRECISION DTEMP
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
C                                       Put card in buffer.
            CALL FITCHM (CARD, IERR)
            IF (IERR.NE.0) GO TO 999
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
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
     *   (Z.GT.0.)) THEN
C                                       Clean beam
         WRITE (MSGTXT,1020) X, Y, Z
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Iterations, product
         I = MAX (1, MIN (4, CATBLK(KITYP)))
         J = I
         IF (CATBLK(KINIT).LE.0) I = 5
         IF (CATBLK(KINIT).LE.0) J = 0
         WRITE (MSGTXT,1025) CATBLK(KINIT), J, PRODS(I)
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       imaging type, xpoff, ypoff
      I = CATBLK(KIITY)
      IF ((I.EQ.1) .OR. (I.EQ.2)) THEN
         IF (ICARD.GE.36) CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         WRITE (MSGTXT,1030) I, CATR(KRXPO), CATR(KRYPO)
         CALL FITCHM (MSGTXT, IERR)
         END IF
C                                       END card
      MSGTXT = 'END     '
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fill and Write record
      IF (ICARD.GT.0) THEN
         IC = 80 * ICARD + 1
         IL = 2880 - IC + 1
         IF (IL.GT.0) THEN
            LINE (IC:2880) = '                  '
            CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
            END IF
         IL = 2880 / (NBITWD / 8)
         CALL COPY (IL, FITBLK, TAPBUF(TBIND))
         CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
         END IF
      ICARD = 0
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
C   tape in the binary FITS formats.
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
     *   I4B, I5, I5A, I5B, I6, I6A, I6B, I7, I7A, I7B, NONZER, NZERO,
     *   BUFFII(MAXIMG)
      REAL      BUFFRR(MAXIMG), FITBRR(2880), TEMP
      DOUBLE PRECISION    BSC, BZE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFFII(1), BUFFRR(1))
      EQUIVALENCE (FITBLK(1), FITBRR(1))
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      NBYB = UVBFSS * 2
      BSC = 1.0D0
      BZE = 0.0D0
      NONZER = 0
      NZERO = 0
      IF (CATSCL.GT.1.0E-30) THEN
         BSC = 1.0D0 / CATSCL
         BZE =  -CATOFF / CATSCL
         END IF
      IF (QUANT.GT.0.0) THEN
         BSC = 1.0 / QUANT
         BZE = 0
         IF (IFMTYP.EQ.3) IFMTYP = 5
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
 50         CALL FITSNC (NX, IFMTYP, BSC, BZE, BUFF(ININD), BUFFII,
     *         BUFFRR, NONZER, NZERO)
            NXY = NX
            IOFF = 1
 55         NXX = MIN (NXY, NVAL-IBL)
C                                       Need new buffer
            IF (NXX.GT.0) GO TO 60
               CALL WRTAPE (IBL, IERR)
               IBL = 0
               IF (IERR.EQ.0) GO TO 55
                  WRITE (MSGTXT,1055) IERR
                  GO TO 980
C                                       Do copy
 60         IF (IFMTYP.LE.2) THEN
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
      IF (IFMTYP.EQ.5) IFMTYP = 3
C                                       Warning:
      IF (NZERO.GT.0) THEN
         TEMP = (100.0D0 * NZERO) / REAL (NZERO + NONZER)
         WRITE (MSGTXT,1100) TEMP
         IF (TEMP.GT.0.1) CALL MSGWRT (6)
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
      SUBROUTINE FITSNC (N, OTYPE, BSC, BZE, RIB, I4, R4, NONZER, NZERO)
C-----------------------------------------------------------------------
C   FITSNC applies any scaling and offsets reqested.
C   Inputs:
C      N        I      Number of pixels
C      OTYPE    I      Type output buffer (I,   I,   R   Rquant 1-4)
C      BSC      D      Multiply buffer by this
C      BZE      D      Then add this
C      RIB      R(N)   Floating in buffer
C   In/out:
C      NONZER   I      Count of non-zero outputs
C      NZERO    I      Count of 0 outputs
C   Out:
C      I4       I(N)   Integer buffer out (OTYPE=1,2)
C      R4       R(N)   Real*4  buffer out (OTYPE=3,4)
C-----------------------------------------------------------------------
      INTEGER   N, OTYPE, I4(*), NONZER, NZERO
      DOUBLE PRECISION    BSC, BZE
      REAL      RIB(*), R4(*)
C
      INTEGER   I, J, MAGIC, MAGIC4, IROUND, MAGIC1
      REAL      TEMP
      DOUBLE PRECISION DTEMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      MAGIC = -32768
      MAGIC4 = -2147483647 - 1
      MAGIC1 = 0
C                                       16 bit integer out
      IF (OTYPE.EQ.1) THEN
         DO 10 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               TEMP = RIB(I) * BSC + BZE
               I4(I) = IROUND (TEMP)
               IF (I4(I).EQ.0) THEN
                  NZERO = NZERO + 1
               ELSE
                  NONZER = NONZER + 1
                  END IF
            ELSE
               I4(I) = MAGIC
               END IF
 10         CONTINUE
C                                       32 bit integer out
      ELSE IF (OTYPE.EQ.2) THEN
         DO 20 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               DTEMP = RIB(I) * BSC + BZE
               IF (DTEMP.GE.0.0D0) THEN
                  I4(I) = DTEMP + 0.5D0
               ELSE
                  I4(I) = DTEMP - 0.5D0
                  END IF
               IF (I4(I).EQ.0) THEN
                  NZERO = NZERO + 1
               ELSE
                  NONZER = NONZER + 1
                  END IF
            ELSE
               I4(I) = MAGIC4
               END IF
 20         CONTINUE
C                                       Floating output
      ELSE IF (OTYPE.EQ.3) THEN
         DO 30 I = 1,N
            R4(I) = RIB(I)
 30         CONTINUE
C                                       8 bit integer out
      ELSE IF (OTYPE.EQ.4) THEN
         DO 40 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               DTEMP = RIB(I) * BSC + BZE
               IF (DTEMP.GE.0.0D0) THEN
                  I4(I) = DTEMP + 0.5D0
               ELSE
                  I4(I) = DTEMP - 0.5D0
                  END IF
               I4(I) = MAX (0, MIN (255, I4(I)))
               IF (I4(I).EQ.0) THEN
                  NZERO = NZERO + 1
               ELSE
                  NONZER = NONZER + 1
                  END IF
            ELSE
               I4(I) = MAGIC1
               END IF
 40         CONTINUE
C                                       Quantized floating out
      ELSE IF (OTYPE.EQ.5) THEN
         DO 50 I = 1,N
            IF (RIB(I).NE.FBLANK) THEN
               DTEMP = RIB(I) * BSC + BZE
               IF (DTEMP.GE.0.0D0) THEN
                  J = DTEMP + 0.5D0
               ELSE
                  J = DTEMP - 0.5D0
                  END IF
               IF (J.EQ.0) THEN
                  NZERO = NZERO + 1
               ELSE
                  NONZER = NONZER + 1
                  END IF
               R4(I) = J / BSC
            ELSE
               R4(I) = FBLANK
               END IF
 50         CONTINUE
         END IF
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
      CHARACTER CHTMP*18, FILTMP*48
      INTEGER   ISLOT, IVOL, IERR, I, JERR, ITT(8), ITRIM
      REAL      TEMP
      HOLLERITH HFDVEC(50)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (HFDVEC, FDVEC)
C-----------------------------------------------------------------------
      IBUFSZ = 2 * UVBFSS
      FIRSTP = .TRUE.
      IFMTYP = ABS(TAMROF) + 0.5
      IF ((IFMTYP.LT.1) .OR. (IFMTYP.GT.4)) IFMTYP = 3
      IF (IFMTYP.NE.3) THEN
         IFMTYP = 3
         MSGTXT = 'FORMAT CHANGED TO IEEE - INTEGER INAPPROPRIATE FOR'
     *      // ' UV DATA'
         CALL MSGWRT (7)
         END IF
      TAMROF = 3.0
C                                       Tell user which map
      WRITE (MSGTXT,1000) USER, IVOL
      CALL H2CHR (18, 1, CATH(KHIMN), CHTMP)
      CALL NAMEST (CHTMP, CATBLK(KIIMS), MSGTXT(41:80), I)
      I = I + 40
      CALL MSGWRT (3)
C                                       Calculate scaling factors.
      CALL CALCSC (ISLOT, IVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       time limits
      TIMBEG = TFIRST
      LSTVIS = 0
      FSTVIS = 1
      IPIECE = 0
C                                       loop over time
 20   CALL FTUVIS (IERR)
      IF (IERR.NE.0) GO TO 999
      IPIECE = IPIECE + 1
      IF (IPIECE.GE.FPIECE) THEN
C                                       disk file make new one
         IF (DODISK) THEN
C                                       close old
            IF (IPIECE.GT.FPIECE) CALL TAPIO ('CLOS', FDVEC, TAPBUF,
     *         TBIND, IERR)
            FILTMP = OUTFIL
            IF (NPIECE.GT.1) THEN
               I = ITRIM (FILTMP) + 1
               IF (IPIECE.LE.9) THEN
                  WRITE (FILTMP(I:),1020) IPIECE
               ELSE IF (IPIECE.LE.99) THEN
                  WRITE (FILTMP(I:),1021) IPIECE
               ELSE
                  WRITE (FILTMP(I:),1022) IPIECE
                  END IF
               CALL CHR2H (48, FILTMP, 1, HFDVEC(7))
               CALL TAPIO ('OPWT', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1025) IERR
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               END IF
            END IF
         WRITE (MSGTXT,1030) FSTVIS, LSTVIS
         CALL MSGWRT (3)
         IF (BYTIME) THEN
            TEMP = TIMBEG
            CALL TODHMS (TEMP, ITT(1))
            TEMP = TIMEND
            CALL TODHMS (TEMP, ITT(5))
            WRITE (MSGTXT,1031) ITT
            CALL MSGWRT (3)
            END IF
C                                       Write header to tape.
         CALL FTUVHE (IERR)
C                                       Writer extra header keywords
         IF (IERR.EQ.0) CALL FITKEY (ISLOT, IVOL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1035) IERR
            GO TO 980
            END IF
C                                       Write history to tape.
         CALL FTUVHI (ISLOT, IVOL, IERR)
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
C                                       Write data to tape.
         CALL FTUVDA (IPIECE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1045) IERR
            GO TO 980
            END IF
C                                       End tape file
         MSGTXT = 'Writing end-of-file mark'
         CALL MSGWRT (2)
         CALL TAPIO ('FLSH', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (BYTIME) TIMBEG = TIMEND
      IF ((IPIECE.LT.NPIECE) .AND. (FSTVIS.LE.LSTVIS)) GO TO 20
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
 1020 FORMAT (I1)
 1021 FORMAT (I2)
 1022 FORMAT (I3)
 1025 FORMAT ('ERROR CREATING NEW DISK FILE:',I7)
 1030 FORMAT ('Writing visibilities',I9,' to',I9)
 1031 FORMAT ('Writing time range',I3,'/',2(I2.2,':'),I2.2,' to',
     *   I3,'/',2(I2.2,':'),I2.2)
 1035 FORMAT ('ERROR WRITING BASIC UV FITS HEADER:',I7)
 1040 FORMAT ('ERROR WRITING UV HISTORY TO FITS HEADER:',I7)
 1045 FORMAT ('ERROR WRITING UV BINARY DATA TO TAPE:',I7)
 1050 FORMAT ('ERROR WRITING EXTENSION TABLES ON TAPE:',I7)
      END
      SUBROUTINE CALCSC (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   CALCSC will calculate various parameters to use when converting
C   uv data from the internal format to FITS tape format.
C   Inputs:
C      COMMON /INPARM/
C   Outputs:
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
C
      CHARACTER CHTM8*8, EXTYPS(6)*2
      DOUBLE PRECISION JDAY
      INTEGER   I, IPTR, LRECIN, FRSRED, BO, IUBIND, NIO, NREAD, IVER,
     *   NVER, IROUND, NANT
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA BO /1/
      DATA EXTYPS /'CL', 'SN', 'TY', 'MC', 'IM', 'BP'/
C-----------------------------------------------------------------------
C                                       Find any "WEIGHT" axis
      ISCMP = CATBLK(KINAX).EQ.1
      CALL AXEFND (8, 'WEIGHT   ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   IERR)
C                                       Find location of parameters in
C                                       header.
      CALL UVPGET (IERR)
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
C                                       baseline conversion?
      CALL FNDEXT ('AN', CATBLK, NVER)
      DOBASL = (ILOCB.LT.0) .AND. (NVER.LT.90)
      IF ((DOBASL) .AND. (MAXANT.GT.255)) THEN
         NANT = 0
         DO 10 I = 1,NVER
            CALL GETANT (IVOL, ISLOT, I, CATBLK, BUFF, IERR)
            IF (IERR.EQ.0) NANT = MAX (NSTNS, NANT)
 10         CONTINUE
         IF (NANT.GT.255) DOBASL = .FALSE.
         END IF
C                                       Blanking
      CATR(KRBLK) = FBLANK
      NUMCOR = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Compressed data OUT
      IF (DOUVCM.GT.0.0) THEN
         CATBLK(KINAX)= 2
C                                       Regular data
      ELSE
         CATBLK(KINAX)= 3
         END IF
C                                       Allowed to divide?
      NPIECE = IROUND (XPIECE)
      IF ((NPIECE.LE.0) .OR. (NPIECE.GT.90)) NPIECE = 1
      IF (NVIS.LE.50*NPIECE) NPIECE = 1
      FPIECE = IROUND (XDROP) + 1
      NVER = 0
      DO 20 I = 1,6
         CALL FNDEXT (EXTYPS(I), CATBLK, IVER)
         NVER = NVER + IVER
 20      CONTINUE
      IF (ISORT(1:1).EQ.'T') THEN
         BYTIME = (NPIECE.GT.1) .AND. (NVER.GT.0)
      ELSE
         BYTIME = .FALSE.
         IF (NVER.NE.0) THEN
            NPIECE = 1
            MSGTXT = 'WRITING 1 PIECE DUE TO SORT ORDER + CAL TABLES'
            CALL MSGWRT (6)
            END IF
         END IF
      IF (FPIECE.GT.NPIECE) FPIECE = 1
C                                       Calculate scaling factor for
C                                       writing to tape and for header
C                                       values.
      CATSCL = 1.0D0
      DO 25 I = 1,20
         HSCAL(I) = 1.0D0
         HZERO(I) = 0.0D0
         HNAME(I) = ' '
         HUNIT(I) = ' '
         HPTR(I) = 0
 25      CONTINUE
      IPTR = 0
      IILOCB = -1
      IF (NRPARM.LE.0) NRPARM = KIPTPN
      DO 30 I = 1,NRPARM
         CALL H2CHR (8, 1, CATH(KHPTP+(I-1)*2), CHTM8)
         IF ((CHTM8.NE.' ') .AND. (CHTM8.NE.'REMOVED') .AND.
     *      (CHTM8.NE.'WEIGHT') .AND. (CHTM8.NE.'SCALE')) THEN
            IF ((DOBASL) .AND. ((CHTM8.EQ.'SUBARRAY') .OR.
     *         (CHTM8.EQ.'ANTENNA1') .OR. (CHTM8.EQ.'ANTENNA2'))) THEN
               IF (IILOCB.LT.0) THEN
                  IPTR = IPTR + 1
                  IILOCB = IPTR
                  HPTR(IPTR) = I
                  HNAME(IPTR) = 'BASELINE'
                  END IF
            ELSE
               IPTR = IPTR + 1
               HPTR(IPTR) = I
               HNAME(IPTR) = CHTM8
               IF ((CHTM8(:2).EQ.'UU') .OR. (CHTM8(:2).EQ.'VV') .OR.
     *            (CHTM8(:2).EQ.'WW')) THEN
                  HSCAL(IPTR) = HSCAL(IPTR) / FREQ
                  HNAME(IPTR)(3:4) = '--'
                  HUNIT(IPTR) = 'SECONDS'
               ELSE IF (CHTM8(:4).EQ.'TIME') THEN
                  HNAME(IPTR) = 'DATE'
                  HUNIT(IPTR) = 'DAYS'
                  CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
                  CALL JULDAY (CHTM8, JDAY)
                  HZERO(IPTR) = JDAY
                  END IF
               END IF
            END IF
 30      CONTINUE
      NOPARM = IPTR
      IF (DOUVCM.GT.0.0) THEN
         HNAME(IPTR+1) = 'WEIGHT'
         HNAME(IPTR+2) = 'SCALE'
         IPTR = IPTR + 2
         END IF
      HNAME(IPTR+1) = 'VISIBILITIES'
      HUNIT(IPTR+1) = 'JY'
C                                       Find first and last times
      TFIRST = -1.E3
      TLAST = 1.E6
      IF (BYTIME) THEN
C                                       Init I/O to uvfile
         NREAD = 1
         LRECIN = LREC
C                                       read first vis
         FRSRED = 0
         CALL UVINIT ('READ', DLUN, DIND, NREAD, FRSRED, LRECIN, 1,
     *      IBUFSZ, TBUFF, BO, IUBIND, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1810) 'INIT FIRST', IERR
            GO TO 990
            END IF
         CALL UVDISK ('READ', DLUN, DIND, TBUFF, NIO, IUBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1810) 'READ FIRST', IERR
            GO TO 990
            END IF
         TFIRST = TBUFF(IUBIND+ILOCT)
C                                       read last vis
         FRSRED = CATBLK(KIGCN) - 1
         CALL UVINIT ('READ', DLUN, DIND, NREAD, FRSRED, LRECIN, 1,
     *      IBUFSZ, TBUFF, BO, IUBIND, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1810) 'INIT LAST', IERR
            GO TO 990
            END IF
         CALL UVDISK ('READ', DLUN, DIND, TBUFF, NIO, IUBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1810) 'READ LAST', IERR
            GO TO 990
            END IF
         TLAST = TBUFF(IUBIND+ILOCT)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATA SET NOT STANDARD ENOUGH FOR ME')
 1810 FORMAT ('TBTIME: UNABLE TO ',A,' INPUT VISIBILITY RECORD, ERROR',
     *   I5)
      END
      SUBROUTINE FTUVIS (IERR)
C-----------------------------------------------------------------------
C   FTUVIS finds the range of vis numbers to be written in this pass
C   Output:
C      IERR     I   Error code from disk IO
C   Output in common:
C      FSTVIS   I   Start vis number
C      LSTVIS   I   Last vis number: is < FSTVIS then no vis are in this
C                   time range
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   BO, VO, NV, LENBU, BIND, IPTR, NIO, J, LL, INC
      REAL      TEMP
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       find out how much UV data we are
C                                       writing
      FSTVIS = LSTVIS + 1
      LSTVIS = NVIS
      TIMEND = TLAST + 1.0D0 / (24.0D0 * 3.6D4)
      IERR = 0
C                                       By pieces
      IF ((NPIECE.GT.1) .AND. (IPIECE.LT.NPIECE-1) .AND.
     *   (FSTVIS.LT.LSTVIS)) THEN
         INC = NVIS / NPIECE
         TEMP = NVIS
         TEMP = TEMP/NPIECE - INC
         LSTVIS = FSTVIS + INC - 1 + (IPIECE+1)*TEMP + 0.5
C                                       need it at a time break
         IF (BYTIME) THEN
            LL = MIN (180, INC/100)
C                                       Init disk IO.
            LENBU = 0
            BO = 1
            VO = LSTVIS - 1 - LL
            NV = NVIS - VO
            CALL UVINIT ('READ', DLUN, DIND, NV, VO, LREC, LENBU,
     *         IBUFSZ, BUFF, BO, BIND, IERR)
            IF (IERR.NE.0) GO TO 999
            LSTVIS = VO
            TIMEND = -1.0
C                                       Loop thru disk file.
 10         CALL UVDISK ('READ', DLUN, DIND, BUFF, NIO, BIND, IERR)
            IF (IERR.GT.0) GO TO 999
            IF ((IERR.EQ.0) .AND. (NIO.GT.0)) THEN
               IPTR = BIND
C                                       check times
               DO 20 J = 1,NIO
                  IF (LSTVIS.EQ.VO) TIMEND = BUFF(IPTR+ILOCT) + 1.0D0 /
     *               (24.0D0 * 3.6D4)
                  IF (BUFF(IPTR+ILOCT).GE.TIMEND) GO TO 999
                  LSTVIS = LSTVIS + 1
                  IPTR = IPTR + LREC
 20               CONTINUE
               GO TO 10
               END IF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE FTUVHE (IERR)
C-----------------------------------------------------------------------
C   FTUVHE causes the new form of null data set header to be written to
C   the output file.
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
      CHARACTER CHEXTN*8, CHBLOK*8, BLKCOM*32, BCOM*48
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DKEY.INC'
      INCLUDE 'INCS:VKEY.INC'
      DATA CHEXTN /'EXTEND  '/
      DATA CHBLOK, BLKCOM /'BLOCKED ', 'Tape may be blocked        '/
C-----------------------------------------------------------------------
      ICARD = 0
C                                       Encode required keywords.
      BCOM = 'Standard FITS file'
      CALL KEYWRD (RWORD(1), RTYPE(1), RPOINT(1), BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      BCOM = ' '
      CALL KEYWRD (RWORD(2), 10, 8, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL KEYWRD (RWORD(3), 10, 2, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      BCOM = 'Signature code for UV data in table'
      CALL KEYWRD (RWORD(4), 10, 777777701, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
      BCOM = 'No data in primary array'
      CALL KEYWRD (RWORD(5), 10, 0, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Always write standard tables
      BCOM = 'All data in tables'
      CALL KEYWRD (CHEXTN, 1, 1, BCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       New FITS BLOCKED = T
      CALL KEYWRD (CHBLOK, 1, 1, BLKCOM, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE KEYWRD (WORD, ITYPE, POINT, COMENT, IERR)
C-----------------------------------------------------------------------
C   This routine will encode a given keyword and the corresponding
C   value in the header into the FITS card format and put that card
C   into the tape output buffer.  If that buffer is full, the output
C   buffer will be written to tape.
C   Inputs:
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
C      COMENT  C*(*)    <= 47 char comment for this card.
C      COMMON /MAPHDR/
C      COMMON /HDRVAL/
C      COMMON /MORPRM/
C   In/out: (In common)
C      ICARD   I         last card in the buffer FITBLK (0-36).
C      TBIND   I         Starting word of current buffer in FITBLK.
C      FITBLK  I(2880)   output work buffer.
C      TAPBUF  I(*)      TAPIO buffers
C   Output:
C      IERR    I         error code. 0=ok, 1=bad.
C-----------------------------------------------------------------------
      CHARACTER WORD*8, COMENT*(*), CARD*80, STR*12, COMMNT*46
      INTEGER   ITYPE, POINT, IERR
      INTEGER   KPNTR(58), PNTR, POFF, NBYT, ISTART, ITEND, ITRIM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KPNTR, KHOBJ)
C-----------------------------------------------------------------------
C                                       Decode POINT.
      COMMNT = COMENT
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
 100     WRITE (CARD,1100) WORD, COMMNT
         GO TO 800
C                                       Numeric.
C                                       Integer.
 200     IF (NBYT.NE.2) GO TO 210
            IF ((ITYPE.EQ.11) .AND. (CATBLK(PNTR+POFF).EQ.0)) GO TO 999
            WRITE (CARD,1200) WORD, CATBLK(PNTR+POFF), COMMNT
            GO TO 800
C                                       Real
 210     IF (NBYT.NE.4) GO TO 220
            IF ((ITYPE.EQ.11) .AND. (CATR(PNTR+POFF).EQ.0.0)) GO TO 999
            WRITE (CARD,1210) WORD, CATR(PNTR+POFF), COMMNT
            GO TO 800
C                                       Double precision
 220        IF ((ITYPE.EQ.11) .AND. (CATD(PNTR+POFF).EQ.0.0D0))
     *         GO TO 999
            WRITE (CARD,1220) WORD, CATD(PNTR+POFF), COMMNT
            GO TO 800
C                                       Character on real boundary.
 300        WRITE (CARD,1300) WORD, COMMNT
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
 400        WRITE (CARD,1300) WORD, COMMNT
            ITEND = NBYT + 12
            ISTART = NBYT * POFF  +  1
            CALL H2CHR (NBYT, ISTART, CATH(PNTR), CARD(12:11+NBYT))
            CARD(ITEND:) = ''''
            GO TO 800
C                                       Image scaling parameter.
 500        IF (WORD.EQ.'BSCALE') WRITE (CARD,1220) WORD, CATSCL, COMMNT
            IF (WORD.EQ.'BZERO') WRITE (CARD,1220) WORD, CATOFF, COMMNT
            GO TO 800
C                                       Get scaling factors from
C                                       common MORPRM.
 600        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, HSCAL(PNTR), COMMNT
            GO TO 800
C                                       Get offsets from MORPRM.
 610        PNTR = MOD (POINT, 100)
            WRITE (CARD,1220) WORD, HZERO(PNTR), COMMNT
            GO TO 800
C                                       Get RP type from MORPRM
 620        PNTR = MOD (POINT, 100)
            WRITE (CARD,1300) WORD, COMMNT
            ITEND = NBYT + 12
            CARD(12:11+NBYT) = HNAME(PNTR)(1:NBYT)
            CARD(ITEND:) = ''' '
            GO TO 800
C                                       Blanking
 630        IF (CATR(PNTR+POFF).EQ.0) GO TO 999
            IF (IFMTYP.EQ.1) WRITE (CARD,1630) WORD
            IF (IFMTYP.EQ.2) WRITE (CARD,1631) WORD
            IF (IFMTYP.EQ.3) WRITE (CARD,1632)
            GO TO 800
C                                       Value given by POINT.
 640        WRITE (CARD,1200) WORD, POINT, COMMNT
C                                       Put card into buffer.
 800  CALL FITCHM (CARD, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A8,'= ',19X,'T / ',A)
 1200 FORMAT (A8,'= ',I20,' / ',A)
 1210 FORMAT (A8,'= ',1PE20.9,' / ',A)
C1220 FORMAT (A8,'= ',1PD20.11,' / ',A)
 1220 FORMAT (A8,'= ',1PE20.11,' / ',A)
 1300 FORMAT (A8,'= ''',19X,' / ',A)
 1630 FORMAT (A8,'= ',14X,'-32768 / Blanked pixel tape value')
 1631 FORMAT (A8,'= ',9X,'-2147483648 / Blanked pixel tape value')
 1632 FORMAT ('COMMENT / IEEE not-a-number used for blanked',
     *   ' f.p. pixels')
      END
      SUBROUTINE FTUVHI (ISLOT, IVOL, IERR)
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
      INTEGER   ISLOT, IVOL, IERR
C
      CHARACTER SRTYPS(3)*4, EXPL(11)*12, CHTM2*2
      INTEGER   IC, IL, I, J1, J2, J, NTYPS
      REAL      X, Y, Z
      LOGICAL   EQUAL
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
C                                       Write general history info.
      CALL FITHIS (ISLOT, IVOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Beam parameters
      X = CATR(KRBMJ)
      Y = CATR(KRBMN)
      Z = CATR(KRBPA)
      IF ((X.GT.0.) .OR. (Y.GT.0.) .OR. (Z.NE.0.)) THEN
         WRITE (MSGTXT,1000) X, Y, Z
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Explain baseline number
      WRITE (MSGTXT,1010)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write UV specific stuff.
C                                       Sort order:
      WRITE (MSGTXT,1011) CATBLK(KITYP)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
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
 20      WRITE (MSGTXT,1020) CHTM2(I:I), EXPL(J)
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 999
 30      CONTINUE
C                                       history: pieces
      WRITE (MSGTXT,1030) IPIECE, NPIECE
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1031) FSTVIS
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1032) LSTVIS
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Put END card in buffer.
      MSGTXT = 'END     '
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ICARD.GT.0) THEN
         IC = 80 * ICARD + 1
         IL = 2880 - IC + 1
         IF (IL.GT.0) THEN
            LINE (IC:2880) = '                  '
            CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
            END IF
         IL = 2880 / (NBITWD / 8)
         CALL COPY (IL, FITBLK, TAPBUF(TBIND))
         CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
         END IF
      ICARD = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HISTORY AIPS   BMAJ=',1PE12.4,' BMIN=',1PE12.4,
     *   ' BPA=',0PF7.2)
 1010 FORMAT (9X,'/ Where baseline = 256*ant1 + ant2 + (array#-1)/100')
 1011 FORMAT ('HISTORY AIPS   SORT ORDER = ''',A2,'''')
 1020 FORMAT (14X,'/ Where ',A1,' means ',A)
 1030 FORMAT ('HISTORY AIPS   IPIECE=',I2,' NPIECE=',I2,5X,
     *   '/ piece number')
 1031 FORMAT ('HISTORY AIPS   FIRSTVIS=',I12,5X,'/ first vis #')
 1032 FORMAT ('HISTORY AIPS   LASTVIS =',I12,5X,'/ last vis #')
      END
      SUBROUTINE FTUVDA (IVER, IERR)
C-----------------------------------------------------------------------
C   FTUVDA write a standard table header to describe the UV data, then
C   writes the UV data into that table.
C   Inputs:
C      IVER     I         UV table section number
C      COMMON /INPARMS/
C      COMMON /MORPRM/
C   Outputs:  (first 4 in Common)
C      ICARD    I         Card number in current tape buffer (FITBLK).
C      FITBLK   I(2880)   FITS header block work area
C      TAPBUF   I(*)      TAPIO buffers
C      TBIND    I         TAPBUF(TBIND) is the start of the current tape
C                         buffer containing the next block of data to be
C                         written to tape.
C      IERR     I         error code. 0=ok, 1=error.
C-----------------------------------------------------------------------
      INTEGER   IVER, IERR
C
      CHARACTER COMARR(15)*40, CHTM12*12, TSTLBL(13)*8, CNUM*2,
     *   FRMOUT*8, TYPTAB*8, SRTYPS(3)*4, EXPL(11)*12, TWORD(35)*8,
     *   TMWORD*8, CHNCOL*3
      INTEGER   I, NWORDS, II, NUMCOM, COMPNT(13), COMNDX, COMLOP, IC,
     *   INDEX, NCOL, IL, BO, VO, NV, LENBU, BIND, IPTR, NIO, J, NEXT,
     *   K, NCOPY, IBLANK, NP, IP, NTYPS, J1, J2
      REAL      RBUF(20), MAXWT, MAXVIS, SCL, XR, XI
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INTEGER   ITBUFF(UVBFSS)
      EQUIVALENCE (ITBUFF, TBUFF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DKEY.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:VKEY.INC'
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
      DATA TYPTAB /'BINTABLE'/
      DATA IBLANK /-32767/
      DATA NTYPS, SRTYPS /11, 'BTUV','WRPX','YZM '/
      DATA EXPL /'BASELINE NUM', 'TIME (IAT)  ',
     *           'U VIS. COORD', 'V VIS. COORD',
     *           'W VIS. COORD', 'BASELINE LEN',
     *           'BASELINE PA ', 'DESC ABS(U) ',
     *           'DESC ABS(V) ', 'ASCE ABS(U) ',
     *           'ASCE ABS(V) '/
C                                       Axis keywords.
      DATA TWORD /'1CTYP   ','1CRVL   ','1CDLT   ','1CRPX   ',
     *            '1CROT   ','2CTYP   ','2CRVL   ','2CDLT   ',
     *            '2CRPX   ','2CROT   ','3CTYP   ','3CRVL   ',
     *            '3CDLT   ','3CRPX   ','3CROT   ','4CTYP   ',
     *            '4CRVL   ','4CDLT   ','4CRPX   ','4CROT   ',
     *            '5CTYP   ','5CRVL   ','5CDLT   ','5CRPX   ',
     *            '5CROT   ','6CTYP   ','6CRVL   ','6CDLT   ',
     *            '6CRPX   ','6CROT   ','7CTYP   ','7CRVL   ',
     *            '7CDLT   ','7CRPX   ','7CROT   '/
C-----------------------------------------------------------------------
      MSGTXT = 'Writing binary table of type UV'
      CALL MSGWRT (2)
      IF (DOUVCM.GT.0.0) THEN
         II = 4 * (NOPARM+2) + 2 * 2 * NUMCOR
         NCOL = NOPARM + 3
      ELSE
         II = 4 * NOPARM + 4 * 3 * NUMCOR
         NCOL = NOPARM + 1
         END IF
C
      ICARD = 0
C                                       Encode required keywords.
      WRITE (MSGTXT,1000) TYPTAB
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1001)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1002)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1003) II
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      II = LSTVIS - FSTVIS + 1
      WRITE (MSGTXT,1004) II
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1005)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1006)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1007) NCOL
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1008) 'UV'
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1009) IVER
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (NCOL.LT.10) THEN
         WRITE (CHNCOL,1010) NCOL
      ELSE IF (NCOL.LT.100) THEN
         WRITE (CHNCOL,1011) NCOL
      ELSE
         WRITE (CHNCOL,1012) NCOL
         END IF
      DO 30 I = 1,NCOL
         IF (I.LT.10) THEN
            WRITE (CNUM,1010) I
         ELSE
            WRITE (CNUM,1011) I
            END IF
C                                       TFORMn
         FRMOUT = '1E'
         IF (I.EQ.NCOL) THEN
            IF (DOUVCM.GT.0.0) THEN
               II= 2 * NUMCOR
               WRITE (FRMOUT,1015) II, 'I'
            ELSE
               II = 3 * NUMCOR
               WRITE (FRMOUT,1015) II, 'E'
               END IF
            END IF
         CALL CHTRIM (FRMOUT, 8, FRMOUT, II)
         WRITE (MSGTXT,1020) CNUM, FRMOUT, I
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       TTYPEn
         WRITE (MSGTXT,1021) CNUM, HNAME(I), I
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       TUNITn
         WRITE (MSGTXT,1022) CNUM, HUNIT(I), I
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       TSCALn
         IF (HSCAL(I).NE.1.0D0) THEN
            WRITE (MSGTXT,1023) CNUM, HSCAL(I), I
            CALL FITCHM (MSGTXT, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
C                                       TZEROn
         IF (HZERO(I).NE.0.0D0) THEN
            WRITE (MSGTXT,1024) CNUM, HZERO(I), I
            CALL FITCHM (MSGTXT, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
 30      CONTINUE
      IF (DOUVCM.GT.0.0) THEN
         WRITE (MSGTXT,1025) CNUM, IBLANK
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      BO = CATBLK(KIDIM)
      WRITE (MSGTXT,1030) CNUM, (CATBLK(KINAX+I-1), I = 1,BO)
      CALL CHPACK (MSGTXT(13:), ' ', I)
      MSGTXT(I+12:) = ')'''
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      MSGTXT = ' '
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Encode axis keywords.
      NWORDS = 5 * CATBLK(KIDIM)
      DO 40 I = 1,NWORDS
C                                       Find comment for label
         INDEX = ((I-1) / 5) * 2 + KHCTP
         COMNDX = 1
         IF (MOD(I, 5) .EQ. 1) THEN
            DO 35 COMLOP = 1,6
               CALL H2CHR (8, 1, CATH(INDEX), CHTM12)
               IF (TSTLBL(COMLOP)(1:8).EQ. CHTM12(1:8))
     *            COMNDX = COMPNT(COMLOP)
 35            CONTINUE
C                                       Correct Stokes' label if nec.
            IF ((COMNDX.EQ.3) .AND. (ICOR0.LT.0)) COMNDX = 4
            END IF
         TMWORD = TWORD(I)
         TMWORD(6:) = CHNCOL
         CALL KEYWRD (TMWORD, ATYPE(I), APOINT(I), COMARR(COMNDX), IERR)
         IF (IERR.NE.0) GO TO 900
 40      CONTINUE
C                                       Encode normal keywords.
      NWORDS = NNT - 2
      DO 50 I = 1,NWORDS
         CALL KEYWRD (NWORD(I), NTYPE(I), NPOINT(I), NCOM(I), IERR)
         IF (IERR.NE.0) GO TO 999
 50      CONTINUE
C                                       Write UV specific stuff.
C                                       Sort order:
      WRITE (MSGTXT,1050) CATBLK(KITYP)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Explain this mess
      CALL H2CHR (2, 1, CATH(KITYP), CNUM)
      DO 65 I = 1,2
         DO 55 J = 1,NTYPS
            J1 = MOD (J-1, 4) + 1
            J2 = (J-1) / 4 + 1
            IF (CNUM(I:I).EQ.SRTYPS(J2)(J1:J1)) GO TO 60
 55         CONTINUE
         GO TO 65
 60      WRITE (MSGTXT,1060) CNUM(I:I), EXPL(J)
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
 65      CONTINUE
C                                       name
      WRITE (MSGTXT,1065) CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), MSGTXT(24:35))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), MSGTXT(47:52))
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       history: user number
      WRITE (MSGTXT,1066) CATBLK(KIIMU)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       history: pieces
      WRITE (MSGTXT,1070) IVER, NPIECE
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1071) FSTVIS
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1072) LSTVIS
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       END card.
      MSGTXT = 'END   '
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Blank fill rest of block.
      IF (ICARD.GT.0) THEN
         IC = 80 * ICARD  + 1
         IL = 2881 - IC
         IF (IL.GT.0) THEN
            LINE (IC:2880) = '                  '
            CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
            END IF
         CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Read through data writing the
C                                       FITS uv table out
C                                       Init disk IO.
      LENBU = 0
      BO = 1
      VO = FSTVIS - 1
      NV = LSTVIS - VO
      CALL UVINIT ('READ', DLUN, DIND, NV, VO, LREC, LENBU, IBUFSZ,
     *   BUFF, BO, BIND, IERR)

      IF (IERR.NE.0) GO TO 999
      NEXT = 1
      BO = NUMCOR * 3
C                                       Loop thru disk file.
 100  CALL UVDISK ('READ', DLUN, DIND, BUFF, NIO, BIND, IERR)
      IF (IERR.GT.0) GO TO 999
      IF ((IERR.EQ.0) .AND. (NIO.GT.0)) THEN
         IPTR = BIND
C                                       check times
         DO 200 J = 1,NIO
            DO 110 I = 1,NOPARM
               IF (I.EQ.IILOCB) THEN
                  RBUF(I) = BUFF(IPTR+ILOCA1)*256.0 + BUFF(IPTR+ILOCA2)
     *               + (BUFF(IPTR+ILOCSA) - 1.0) / 100.0
               ELSE
                  K = HPTR(I)
                  RBUF(I) = BUFF(IPTR+K-1)
                  END IF
 110           CONTINUE
            IF (ISCMP) THEN
               CALL ZUVXPN (NUMCOR, BUFF(IPTR+NRPARM),
     *            BUFF(IPTR+KLOCWT), TBUFF)
            ELSE
               CALL RCOPY (BO, BUFF(IPTR+NRPARM), TBUFF)
               END IF
C                                       uncompressed out
            IF (DOUVCM.LE.0.0) THEN
               CALL ZRLRUV (NOPARM, 1, RBUF, RBUF)
               NCOPY = 4 * NOPARM
               CALL PTF3D (FDVEC, TBIND, NEXT, RBUF, TAPBUF, NCOPY,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
               CALL ZRLRUV (BO, 1, TBUFF, TBUFF)
               NCOPY = 4 * BO
               CALL PTF3D (FDVEC, TBIND, NEXT, TBUFF, TAPBUF, NCOPY,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
C                                       compressed out
            ELSE
C                                       Find max wt, vis
               MAXWT = 0.0
               MAXVIS = 0.0
               IP = 0
               DO 120 I = 1,NUMCOR
                  IF (TBUFF(3+IP).GT.0.0) THEN
                     MAXVIS = MAX (MAXVIS, ABS (TBUFF(1+IP)),
     *                  ABS (TBUFF(2+IP)))
                     MAXWT = MAX (MAXWT, TBUFF(3+IP))
                     END IF
                  IP = IP + 3
 120              CONTINUE
C                                       Set weight and scaling
C                                       parameters
               RBUF(NOPARM+1) = MAXWT
               RBUF(NOPARM+2) = MAXVIS / 32760.
               SCL = 1.0
               IF (RBUF(NOPARM+2).GT.1.0E-10) SCL = 1.0 / RBUF(NOPARM+2)
C                                       Scale to integer
               NP = 0
               IP = 0
               DO 130 I = 1,NUMCOR
                  IF (TBUFF(3+IP).GT.0.0) THEN
                     XR = TBUFF(1+IP) * SCL
                     XI = TBUFF(2+IP) * SCL
                     ITBUFF(NP+1) = XR + SIGN (0.5, XR)
                     ITBUFF(NP+2) = XI + SIGN (0.5, XI)
                  ELSE
                     ITBUFF(NP+1) = IBLANK
                     ITBUFF(NP+2) = IBLANK
                     END IF
                  NP = NP + 2
                  IP = IP + 3
 130              CONTINUE
               NCOPY = NOPARM + 2
               CALL ZRLRUV (NCOPY, 1, RBUF, RBUF)
               NCOPY = 4 * NCOPY
               CALL PTF3D (FDVEC, TBIND, NEXT, RBUF, TAPBUF, NCOPY,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
               NCOPY = NUMCOR * 2
               CALL ZILI16 (NCOPY, ITBUFF, 1, ITBUFF)
               NCOPY = 2 * NCOPY
               CALL PTF3D (FDVEC, TBIND, NEXT, ITBUFF, TAPBUF, NCOPY,
     *            IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            IPTR = IPTR + LREC
 200        CONTINUE
         GO TO 100
         END IF
C                                       Last record flush
      IF (NEXT.GT.1) THEN
         CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C
      GO TO 999
C                                       Error on I/O
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XTENSION= ',1H',A8,1H',11X,'/ Extension type')
 1001 FORMAT ('BITPIX  =',20X,'8',1X,'/ Binary data')
 1002 FORMAT ('NAXIS   =',20X,'2',1X,'/ Table is a matrix')
 1003 FORMAT ('NAXIS1  =',13X,I8,1X,'/ Width of table in bytes')
 1004 FORMAT ('NAXIS2  =',13X,I8,1X,'/ Number of entries in table')
 1005 FORMAT ('PCOUNT  =',20X,'0',1X,'/ Random parameter count')
 1006 FORMAT ('GCOUNT  =',20X,'1',1X,'/ Group count')
 1007 FORMAT ('TFIELDS =',19X,I2,1X,'/ Number of fields in each row')
 1008 FORMAT ('EXTNAME = ''AIPS ',A2,1X,'''',11X,'/ AIPS table file')
 1009 FORMAT ('EXTVER  = ',15X,I5,1X,'/ Version number of table')
 1010 FORMAT (I1,' ')
 1011 FORMAT (I2)
 1012 FORMAT (I3)
 1015 FORMAT (I7,A1)
 1020 FORMAT ('TFORM',A2,' = ''',A8,'''',11X,'/ FORTRAN format',
     *   ' of field',I3)
 1021 FORMAT ('TTYPE',A2,' = ''',A16,'''',3X,'/ Type (heading)',
     *   ' of field',I3)
 1022 FORMAT ('TUNIT',A2,' = ''',A8,'''',11X,'/ physical units',
     *   ' of field',I3)
 1023 FORMAT ('TSCAL',A2,' = ',1PD20.13,' / scale to physical units',
     *   ' in field',I3)
 1024 FORMAT ('TZERO',A2,' = ',1PD20.13,' / offset to physical units',
     *   ' in field',I3)
 1025 FORMAT ('TNULL',A2,' = ',I20,' / magic value for flagged',
     *   ' data')
 1030 FORMAT ('TDIM',A2,'  = ''(',7(I6,','))
 1050 FORMAT ('HISTORY AIPS   SORT ORDER = ''',A2,'''')
 1060 FORMAT (14X,'/ Where ',A1,' means ',A)
 1065 FORMAT ('HISTORY AIPS   IMNAME=''',12X,''' IMCLASS=''',6X,
     *   ''' IMSEQ=',I4,5X,'/ ')
 1066 FORMAT ('HISTORY AIPS   USERNO=',I5,12X,'/ ')
 1070 FORMAT ('HISTORY AIPS   IPIECE=',I2,' NPIECE=',I2,5X,
     *   '/ piece number')
 1071 FORMAT ('HISTORY AIPS   FIRSTVIS=',I12,5X,'/ first vis #')
 1072 FORMAT ('HISTORY AIPS   LASTVIS =',I12,5X,'/ last vis #')
 1900 FORMAT ('FTUVDA: FITS WRITE ERROR',I7)
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
C      FITBLK  I(2880)   header buffer
C      TAPBUF  I(*)      TAPIO buffers
C   Output:
C      IERR    I         error code. 0=ok, 1=bad>
C-----------------------------------------------------------------------
      INTEGER   ISLOT, IVOL, IERR
C
      CHARACTER HISTRY*8, CHTMP*72, HILINE*80, COFITS(2)*72
      HOLLERITH IBUFF(256)
      INTEGER   I, IHLUN, LOC, KT, KD, HIPTR, IL, J, ID(3), IER, ITRIM
      LOGICAL   F
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
      HIPTR = 1
      KT = HITAB(HIPTR+2)
C                                       Let user know.
      MSGTXT = 'Writing HIstory to main FITS header'
      CALL MSGWRT (3)
C                                       Begin HI record loop
      DO 60 KD = 1,KT
         I = MOD (KD-1, NHILPR) + 1
C                                       Read next HI physical record
         IF (I.EQ.1) THEN
            LOC = LOC + 1
            CALL HIIO ('READ', HIPTR, LOC, IBUFF, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1020) IER, LOC
               GO TO 90
               END IF
            END IF
C                                       Copy HI data into buffer
         IL = 5 + NHIWPL * MOD (KD-1, NHILPR)
         HILINE(1:8) = HISTRY
C                                       See if this card already starts
C                                       with word history.
         CALL SPFIL (IBUFF(IL), 72, J)
         CALL H2CHR (72, 1, IBUFF(IL), CHTMP)
C                                       Not AIPS / FITS history
         IF ((CHTMP.NE.COFITS(1)) .AND. (CHTMP.NE.COFITS(2)) .AND.
     *      (CHTMP(1:5).NE.'AIPS ')) THEN
C                                       "HISTORY" in card
            IF (CHTMP(1:8).EQ.HISTRY) THEN
               HILINE(9:72) = CHTMP(9:72)
               HILINE(73:80) = '        '
            ELSE
               HILINE(9:80) = CHTMP
               END IF
C                                       Card to FITBLK
            CALL FITCHM (HILINE, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 60      CONTINUE
C                                       Close HI file
 90   CALL HICLOS (IHLUN, F, IBUFF, IER)
C                                       extra parameters
C                                       ORIGIN keyword
 100  WRITE (MSGTXT,1100) HSTNAM, SYSNAM, RLSNAM
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DATE keyword
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
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       history: names
      WRITE (MSGTXT,1120) CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), MSGTXT(24:35))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), MSGTXT(47:52))
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       history: user number
      WRITE (MSGTXT,1130) CATBLK(KIIMU)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       history quantization
      IF ((.NOT.ISUV) .AND. (QUANT.GT.0.0)) THEN
         WRITE (MSGTXT,1150) QUANT
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       FITS ref 1 and 2
      MSGTXT = 'COMMENT ' // COFITS(1)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
      MSGTXT = 'COMMENT ' // COFITS(2)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 999
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
 1150 FORMAT ('HISTORY FITAB  QUANTIZE=',1PE12.5,5X,
     *   '/ image quantization')
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
      REAL      FITBR4(2880)
      INTEGER   ITFULL, ITMAX, IERR, ITZ
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (FITBLK, FITBR4)
C-----------------------------------------------------------------------
C                                       Write this buffer to tape.
      ITMAX = 720
      IF (IFMTYP.EQ.1) ITMAX = 1440
      IF (IFMTYP.EQ.4) ITMAX = 2880
      ITZ = ITMAX - ITFULL
      IF (ITZ.GT.0) THEN
         IF ((IFMTYP.EQ.3) .OR. (IFMTYP.EQ.5)) THEN
            CALL RFILL (ITZ, 0.0, FITBR4(ITFULL+1))
         ELSE
            CALL FILL (ITZ, 0, FITBLK(ITFULL+1))
            END IF
         END IF
C                                       translate
      IF (IFMTYP.EQ.1) THEN
         CALL ZILI16 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      ELSE IF (IFMTYP.EQ.2) THEN
         CALL ZILI32 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      ELSE IF (IFMTYP.EQ.4) THEN
         CALL ZILI8 (ITMAX, FITBLK, 1, TAPBUF(TBIND))
      ELSE
         CALL ZRLR32 (ITMAX, 1, FITBLK, TAPBUF(TBIND))
         END IF
C                                       write
      CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
      END
      SUBROUTINE FITCHM (TEXT, IERR)
C-----------------------------------------------------------------------
C   FITCHM moves TEXT to FITBLK 80-character strings converting to
C   real world characters and writing the buffer if it is full.
C   Inputs:
C      TEXT     C*80      text line:
C   Input (common):
C      ICARD    I      Last card number writton
C      FITBLK   I(*)   Character buffer
C-----------------------------------------------------------------------
      CHARACTER TEXT*80
      INTEGER   IERR
C
      INTEGER   IC
      INCLUDE 'FITAB.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF (ICARD.GE.36) CALL WRCTAP (IERR)
      IF (IERR.EQ.0) THEN
         ICARD = ICARD + 1
         IC = 80 * ICARD - 79
         CALL ZCLC8 (80, TEXT, IC, FITBLK)
         IF (ICARD.EQ.36) CALL WRCTAP (IERR)
         END IF
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
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
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
         IF (MAXVER.LE.0) GO TO 800
            CALL H2CHR (2, 1, CATH(ITABLE), TTYPE)
C                                       Loop over versions
            DO 780 IVER = 1,MAXVER
C                                       Do table
               CALL EXTWRT (TTYPE, IVER, KVOL, ISLOT, IER)
               IF (IER.NE.0) GO TO 999
 780           CONTINUE
 800        CONTINUE
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
     *   FORM3D*8, FORMXX*8, FORCOD(7)*1, FORM(7)*8, TYPTAB*8,
     *   FRMOUT*8, CHTMP*10, KEYS*24
      HOLLERITH RECH(XBPRSZ), HVALUE(2)
      INTEGER   KVOL, ISLOT, IER
      DOUBLE PRECISION RECORD(XBPRSZ/2), DVALUE, RECRD(XBPRSZ/2), DTIME
      REAL   RLINE(XBPRSZ), RECRR(XBPRSZ), RVALUE, RVALS(2), FKEY(2,2)
      INTEGER   NREC, RECI(XBPRSZ), IRNO, JVALUE,
     *   IPAIR, SUMBYT, MXTEST, MAXL
      LOGICAL   EXIST, TABLE, RECL(XBPRSZ), FITASC, DOTIME
      INTEGER   IVER, LUN, IFORMT(128), BUFFER(512), IC, I, II, ITRIM,
     *   IERR, IFLEN(128), ITLEN(7), ITYPE, NKEY, NCOL, DATP(128,2),
     *   ICRDL, INCRD, IL, IRCODE, IDUM,ILEN, IBCOL(128), MINLEN, IOFF,
     *   ILEN0, JJ, SRTORD, NUMFOR, NUMBYT(7), IRNO1, IRNO2, KOLS, LRNO,
     *   RTYPE, NCOPY, NEXT, TCOUNT(128), TPTYPE(128), TOFF(128),
     *   ITNCOL, FLAGD, RECOUT, KEY(2,2), KEYSUB(2,2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITAB.INC'
      INCLUDE 'FITAB2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (DVALUE, HVALUE, RVALS)
      EQUIVALENCE (RECORD, RECRD, RECRR, RECI, RECL, RECH)
      DATA FORCOD /'D','E','A','J','L','I','X'/
      DATA NUMBYT / 8, 4,  1,  4,  1,  2,  1/
      DATA MAXL /XBPRSZ/
      DATA LUN /28/
      DATA FORM /'D24.15  ','E15.6   ','        ',
     *   'I12     ','A1      ', 'I6      ','        '/
      DATA ITLEN /24, 15, 0, 12, 0, 6, 0/
      DATA TYPTAB /'BINTABLE'/
      DATA FKEY /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      IER = 0
      ILEN0 = 160
      DOTIME = (TTYPE.EQ.'CL') .OR. (TTYPE.EQ.'SN') .OR. (TTYPE.EQ.'TY')
     *   .OR. (TTYPE.EQ.'MC') .OR. (TTYPE.EQ.'IM') .OR. (TTYPE.EQ.'BP')
      DOTIME = DOTIME .AND. BYTIME
C                                       See if we have a table.
      CALL ISTAB (TTYPE, KVOL, ISLOT, IVER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IERR)
      IF (.NOT.EXIST) GO TO 999
      IF (.NOT.TABLE) GO TO 999
      CALL TABINI ('READ', TTYPE, KVOL, ISLOT, IVER, CATBLK, LUN, NKEY,
     *   IDUM, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      NREC = BUFFER(5)
      IF (DOTIME) THEN
         KEYS = 'TIME'
         CALL FNDCOL (1, KEYS, 8, KOLS, BUFFER, KOLS, IERR)
         IF (IERR.NE.0) DOTIME = .FALSE.
C                                       Sort to time order if necessary
         IF (BUFFER(43).NE.KOLS) THEN
C                                       Close table
            CALL TABIO ('CLOS', 0, IRNO, RECI, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            KEY(1,1) = KOLS
            KEY(2,1) = 0
            KEY(1,2) = KOLS
            KEY(2,2) = 0
C                                       Sort
            CALL TABSRT (KVOL, ISLOT, TTYPE, IVER, IVER, KEY, KEYSUB,
     *         FKEY, BUFFER, CATBLK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Reopen table
            CALL TABINI ('READ', TTYPE, KVOL, ISLOT, IVER, CATBLK, LUN,
     *         NKEY, IDUM, NCOL, DATP, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Count the unflagged records.
      FLAGD = 0
      IRNO2 = 1
      IRNO1 = 0
      RECOUT = 0
      DTIME = (TIMBEG + TIMEND) / 2.0
      DO 10 IRNO = 1,NREC
         CALL TABIO ('READ', IRCODE, IRNO, RECI, BUFFER, IERR)
C                                       IERR=-1 is flagged row.
         IF (IERR.EQ.-1) THEN
            IF (IRNO1.GT.0) FLAGD = FLAGD + 1
         ELSE IF (IERR.NE.0) THEN
            GO TO 900
         ELSE IF (DOTIME) THEN
            CALL GETCOL (IRNO, KOLS, DATP, LRNO, BUFFER, RTYPE, DVALUE,
     *         RECI, IERR)
            IF (MOD(RTYPE,10).EQ.1) THEN
               DTIME = DVALUE
            ELSE
               DTIME = RVALS(1)
               END IF
            IF ((DTIME.GE.TIMBEG) .AND. (DTIME.LT.TIMEND)) THEN
               IF (IRNO1.EQ.0) IRNO1 = IRNO
               IRNO2 = IRNO
               RECOUT = RECOUT + 1
            ELSE IF (DTIME.GE.TIMEND) THEN
               GO TO 15
               END IF
         ELSE
            IF (IRNO1.EQ.0) IRNO1 = IRNO
            IRNO2 = IRNO
            RECOUT = RECOUT + 1
            END IF
 10      CONTINUE
 15   IERR = 0
C                                       Let the user know.
C
      IRNO = RECOUT + FLAGD
      IF (FLAGD.GT.0) THEN
         WRITE (MSGTXT,1010) RECOUT
         CALL MSGWRT (4)
         WRITE (MSGTXT,1011) IRNO, TTYPE, IVER
         CALL MSGWRT (4)
         END IF
C                                       Everything you need to know
C                                       about the columns.
      CALL GETHUT (NCOL, DATP, BUFFER, COLHDR, UNITS, IFORMT, IFLEN,
     *   IERR)
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
         GO TO 920
         END IF
C                                       Table header
      ICARD = 0
C                                       Tell user table and type
      WRITE (MSGTXT,1015) TTYPE
      CALL MSGWRT (3)
      WRITE (MSGTXT,1050) TYPTAB
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1051)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1052)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1053) SUMBYT
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1054) RECOUT
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1055)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1056)
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1057) NCOL
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1058) TTYPE
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1059) IVER
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
      DO 80 I = 1,NCOL
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
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (I.LT.10) WRITE (MSGTXT,1072) I, COLHDR(I)(1:16), I
         IF (I.GE.10) WRITE (MSGTXT,1073) I, COLHDR(I)(1:16), I
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (I.LT.10) WRITE (MSGTXT,1074) I, UNITS(I), I
         IF (I.GE.10) WRITE (MSGTXT,1075) I, UNITS(I), I
         CALL FITCHM (MSGTXT, IERR)
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
C
 230        CALL FITCHM (MSGTXT, IERR)
            IF (IERR.NE.0) GO TO 900
 240        CONTINUE
         END IF
C                                       Sort order.
      SRTORD = BUFFER(43)
      IF (SRTORD.NE.0) THEN
         WRITE (MSGTXT,1230) SRTORD
         CALL FITCHM (MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       END card.
      MSGTXT = 'END '
      CALL FITCHM (MSGTXT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Blank fill rest of block.
      IF (ICARD.GT.0) THEN
         IC = 80 * ICARD  + 1
         IL = 2881 - IC
         IF (IL.GT.0) THEN
            LINE (IC:2880) = '                  '
            CALL ZCLC8 (IL, LINE(IC:2880), IC, FITBLK)
            END IF
         CALL WRCTAP (IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       3-D table
      IRCODE = 0
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
      IF ((IRNO1.GT.0) .AND. (IRNO2.GE.IRNO1)) THEN
         DO 740 IRNO = IRNO1,IRNO2
            CALL TABIO ('READ', IRCODE, IRNO, RECI, BUFFER, IERR)
            IF (IERR.EQ.0) THEN
C                                       Use RLINE for scratch output
               DO 700 II = 1,ITNCOL
                  ITYPE = TPTYPE(II)
                  ILEN = TCOUNT(II)
                  IOFF = TOFF(II)
C                                       Double precision.
                  IF (ITYPE.EQ.1) THEN
                     CALL ZRLR64 (ILEN, 1, RECRD(IOFF), RLINE)
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
 650                    CONTINUE
                     CALL ZCLC8 (ILEN, LINE, 1, RLINE)
C                                       Integer (short no longer valid)
                  ELSE IF (ITYPE.EQ.6) THEN
                     IER = 5
                     MSGTXT = 'SHORT INTEGERS NO LONGER VALID'
                     CALL MSGWRT (8)
                     GO TO 920
C                                       Bit array
                  ELSE IF (ITYPE.EQ.7) THEN
                     CALL ZXLX8 (ILEN, RECI(IOFF), RLINE)
                     END IF
C                                       Copy to output
                  NCOPY = NUMBYT(ITYPE) * ILEN
                  IF (ITYPE.EQ.7) NCOPY = 1 + (ILEN-1) / (NBITWD/2)
                  CALL PTF3D (FDVEC, TBIND, NEXT, RLINE, TAPBUF, NCOPY,
     *               IERR)
                  IF (IERR.NE.0) GO TO 900
 700              CONTINUE
               END IF
 740        CONTINUE
C                                       Last record flush
         IF (NEXT.GT.1) THEN
            CALL TAPIO ('WRIT', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
      GO TO 920
C                                       Error on I/O
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
      IER = IERR
C
 920  CALL TABIO ('CLOS', IRCODE, IRNO, RECI, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('EXTWRT: WRITING ',I6,' UNFLAGGED ROWS IN ALL OUT OF ')
 1011 FORMAT (8X,I6,' TOTAL FOR TABLE ',A2,' VERSION ',I3)
 1015 FORMAT ('Writing binary table of type ',A2)
 1040 FORMAT ('BUFFER NOT BIG ENOUGH FOR NEEDED BYTES=',I8)
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
      IF (IERR.EQ.0) GO TO 100
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
C                                       Get number of Channels
 100  NUMCH = BUFFER(5)
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
         IF (DOREAD) GO TO 160
            RECI(NOKOL) = I
            RECI(SBKOL) = ISBAND(I)
            RECD(OFFKOL) = FOFF(I)
C                                       Process record.
 160     CALL TABIO (OPCODE, 0, ICHRNO, RECI, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 999
         IF (IERR.LT.0) GO TO 200
C                                       If READ pick data from RECORD.
         IF (.NOT.DOREAD) GO TO 200
            J = RECI(NOKOL)
            FOFF(J) = RECD(OFFKOL)
            ISBAND(J) = RECI(SBKOL)
 200        CONTINUE
C                                       Close
      CALL TABIO ('CLOS', 0, ICHRNO, RECI, BUFFER, IERR)
      GO TO 999
C                                       Case of no or 1 IF axis.
C                                       If write just return
 500  IF (.NOT.DOREAD) GO TO 999
      NIF = 1
      FOFF(1) = 0.0D0
      ISBAND(1) = 0
C
 999  RETURN
      END
