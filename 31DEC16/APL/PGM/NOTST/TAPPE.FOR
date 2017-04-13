LOCAL INCLUDE 'TAPPE.INC'
C                                       Local include for TAPPE
      INTEGER   SEQI, SEQO, INVER1, INVER2, OUTVER, CNOI, CNOO, DISKI,
     *   DISKO, CATI(256), MAXVER, NROW1, NROW2, NROWO
      HOLLERITH XNAMEI(3), XCLASI(2), XXTYPE, XNAMEO(3), XCLASO(2)
      CHARACTER NAMEI*12, CLASSI*6, TTYPE*2, NAMEO*12, CLASSO*6
      REAL      XSEQI, XDISKI, XVER1, XSEQO, XDISKO, XVER2, BADD(10)
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQI, XDISKI, XXTYPE, XVER1,
     *   XNAMEO, XCLASO, XSEQO, XDISKO, XVER2, BADD
      COMMON /OTHER/ CATI, SEQI, SEQO, INVER1, INVER2, OUTVER, CNOI,
     *   CNOO, DISKI, DISKO, MAXVER, NROW1, NROW2, NROWO
      COMMON /CHRCOM/ NAMEI, CLASSI, TTYPE, NAMEO, CLASSO
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM TAPPE
C-----------------------------------------------------------------------
C! Appends two tables and discards duplicates
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2011, 2015
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
C   Task TAPPE sorts AIPS tables to bring all rows at the same control
C   values together, then it sums them, and finally it resorts the file
C   into the original order.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAMEI         File name.
C      INCLASS        CLASI         File class.
C      INSEQ          SEQI          File sequence number.
C      INDISK         DISKI         Disk volume on which file resides.
C      INEXT          TTYPE         Extension file type
C      INVER          INVER1        Input file input version number
C      OUTNAME        NAMEO         Output file name
C      OUTCLASS       CLASSO        Output file class
C      OUTSEQ         SEQO          Output file seq
C      OUTDISK        DISKO         Output file disk
C      IN2VER         INVER2        Output file 2nd input version #
C      OUTVER         OUTVER        Output file version number
C      BADDISK        IBADD(10)     Disks to avoid for scratch.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   IRET, SCRBUF(512)
      INCLUDE 'TAPPE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Set file info
      CALL TAPPEI (SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do operation
      CALL TAPPED (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History and clean up
      CALL TAPPEH (IRET, SCRBUF)
C                                       Write end message
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE TAPPEI (SCRBUF, IRET)
C-----------------------------------------------------------------------
C   TAPPEI performs initialization for TAPPE
C   Output:
C      SCRBUF   I(*)     Scratch
C      IRET     I        Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IRET, SCRBUF(*)
C
      CHARACTER PRGM*6, PTYPE*2, STAT*4
      INTEGER   NPARM, IROUND, I
      INCLUDE 'TAPPE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGM /'TAPPE'/
C-----------------------------------------------------------------------
C                                       Init I/O, parameters
      NPARM = 27
      CALL SETUP (PRGM, NPARM, XNAMEI, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 990
      SEQI = IROUND (XSEQI)
      DISKI = IROUND (XDISKI)
      INVER1 = IROUND (XVER1)
      SEQO = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
      INVER2 = IROUND (XVER2)
      OUTVER = 0
      CALL H2CHR (12, 1, XNAMEI, NAMEI)
      CALL H2CHR (6, 1, XCLASI, CLASSI)
      CALL H2CHR (12, 1, XNAMEO, NAMEO)
      CALL H2CHR (6, 1, XCLASO, CLASSO)
      CALL H2CHR (2, 1, XXTYPE, TTYPE)
      IF (TTYPE.EQ.'  ') TTYPE = 'TA'
C                                       Open file and get CATBLK.
C                                       Get CATBLK from old file.
      CNOI = 1
      PTYPE = '  '
      CALL CATDIR ('SRCH', DISKI, CNOI, NAMEI, CLASSI, SEQI, PTYPE,
     *   NLUSER, STAT, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, NAMEI, CLASSI, SEQI, DISKI, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI, CNOI, CATI, 'REST', SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       BADDISK
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
C                                       find version numbers
      CALL FXHDEX (CATI)
      CALL FNDEXT (TTYPE, CATI, MAXVER)
      IF ((INVER1.LE.0) .OR. (INVER1.GT.MAXVER)) INVER1 = MAXVER
C                                       output file
      IF (NAMEO.EQ.' ') NAMEO = NAMEI
      IF (CLASSO.EQ.' ') CLASSO = CLASSI
      IF (SEQO.LE.0) SEQO = SEQI
      IF (DISKO.LE.0) DISKO = DISKI
      CNOO = 1
      PTYPE = '  '
      CALL CATDIR ('SRCH', DISKO, CNOO, NAMEO, CLASSO, SEQO, PTYPE,
     *   NLUSER, STAT, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, NAMEO, CLASSO, SEQO, DISKO, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKO, CNOO, CATBLK, 'WRIT', SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Mark in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 1
C                                       output file versions
      CALL FXHDEX (CATBLK)
      CALL FNDEXT (TTYPE, CATBLK, MAXVER)
      IF ((INVER2.LE.0) .OR. (INVER2.GT.MAXVER)) INVER2 = MAXVER
      IF ((OUTVER.LE.0) .OR. (OUTVER.GT.MAXVER)) OUTVER = MAXVER + 1
C                                       input status?
      IF ((DISKI.NE.DISKO) .OR. (CNOI.NE.CNOO)) THEN
         CALL CATIO ('READ', DISKI, CNOI, CATI, 'READ', SCRBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Mark in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI
         FCNO(NCFILE) = CNOI
         FRW(NCFILE) = 1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1010 FORMAT ('ERROR',I3,' READ HEADER, SETTING STATUS')
      END
      SUBROUTINE TAPPED (IRET)
C-----------------------------------------------------------------------
C   TAPPED performs the operation: 2 copies, sort, merge
C   Output:
C      IRET     I        Error code: 0 -> okay
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NSTD, NSCOL
      PARAMETER (NSTD=34, NSCOL=6)
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   TABUF1(512), TABUFO(512), DATP(128,2), NKEY, NCOL,
     *   NEXT, LUN1, LUN2, LUNO, IREC1, MREC, KLOCS(100), KEYTYP(100),
     *   NSEL, J, KEY(2,2), NWORDS, KEYSUB(2,2), STDCOL(NSTD,2),
     *   ANTCOL(NSCOL,3), HDRSRT(2), TABUF2(512), IREC2, IP1, IP2,
     *   IPCOL(2,2), STRLEN(2,2), COLTYP(2,2), IBUF1(UVBFSL),
     *   IBUF2(UVBFSL)
      CHARACTER KEYW(100)*8, STDTYP(NSTD)*2, ANTTYP(NSCOL)*2
      REAL      FKEY(2,2), DBUF1(UVBFSL), DBUF2(UVBFSL)
      LOGICAL   READ1, READ2
      EQUIVALENCE (IBUF1, DBUF1), (IBUF2, DBUF2)
      INCLUDE 'TAPPE.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2, LUNO /32, 33, 34/
      DATA KEYSUB /4*1/
      DATA STDTYP /'AT','BT','CA','CL','GA','IM','NX','OF','OT','PC',
     *   'PH','SN','TS','TY','CS', 'WX','CM','MC', 'FQ','PO','CQ','FR',
     *   'GC','GN','GP','SO','SU','CT', 'AN', 'CC', 'MF','AG', 'BP',
     *   'OB'/
      DATA STDCOL /28*1, 4,2,4,5,1,3,  15*4, 3*3, 10*2, 2,3,5,2,5,1/
      DATA ANTTYP /'FL','BS','BL','BC','FG','FC'/
      DATA ANTCOL /5,1,1,1,5,3, 3,3,4,4,4,4, 3,3,5,4,4,4/
C-----------------------------------------------------------------------
C                                       Open input file 1
      CALL TABINI ('READ', TTYPE, DISKI, CNOI, INVER1, CATI, LUN1, NKEY,
     *   MREC, NCOL, DATP, TABUF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN 1ST INPUT TABLE'
         GO TO 990
         END IF
      NROW1 = TABUF1(5)
      NWORDS = TABUF1(8) - 1
C                                       set sort info
      DO 10 J = 1,2
         FKEY(1,J) = 1.0
         FKEY(2,J) = 0.0
         IF (TABUF1(42+J).LT.0) THEN
            FKEY(1,J) = -1.0
            TABUF1(42+J) = -TABUF1(42+J)
            END IF
         KEY(1,J) = TABUF1(42+J)
         IF (KEY(1,J).GT.256) KEY(1,J) = 256 - TABUF1(42+J)
         KEY(2,J) = 0
 10      CONTINUE
C                                       override for standard types
      DO 15 J = 1,NSTD
         IF (TTYPE.EQ.STDTYP(J)) THEN
            KEY(1,1) = STDCOL(J,1)
            KEY(1,2) = STDCOL(J,2)
            FKEY(1,1) = 1.0
            FKEY(1,2) = 1.0
            END IF
 15      CONTINUE
      DO 20 J = 1,NSCOL
         IF (TTYPE.EQ.ANTTYP(J)) THEN
            KEY(1,1) = ANTCOL(J,1)
            FKEY(1,1) = 1.0
            KEY(1,2) = ANTCOL(J,2)
            FKEY(1,2) = 256.0
            KEY(2,2) = ANTCOL(J,3)
            FKEY(2,2) = 1.0
            IF (KEY(1,2).EQ.KEY(2,2)) KEYSUB(2,2) = 2
            END IF
 20      CONTINUE
C                                       Set sort order
      HDRSRT(1) = KEY(1,1)
      IF (HDRSRT(1).LT.0) HDRSRT(1) = 256 - KEY(1,1)
      HDRSRT(2) = KEY(1,2)
      IF (HDRSRT(2).LT.0) HDRSRT(2) = 256 - KEY(1,2)
      IF (FKEY(1,1).LT.0.0) HDRSRT(1) = -HDRSRT(1)
      IF (FKEY(1,2).LT.0.0) HDRSRT(2) = -HDRSRT(2)
C                                       Get column types
      DO 30 J = 1,2
         IP1 = ABS (KEY(1,J))
         IPCOL(1,J) = 1
         IF (IP1.GT.0) IPCOL(1,J) = DATP(IP1,1)
         IP2 = ABS (KEY(2,J))
         IPCOL(2,J) = 1
         IF (IP2.GT.0) IPCOL(2,J) = DATP(IP2,1)
C                                       Check factors.
         IF (FKEY(1,J).EQ.0.0) FKEY(1,J) = 1.0
         IF (FKEY(2,J).EQ.0.0) FKEY(2,J) = 1.0
C                                       Column out of range.
         IF (((IP1.GE.1) .AND. (IP1.LE.NCOL)) .AND.
     *      ((IP2.GE.0) .AND. (IP2.LE.NCOL))) GO TO 25
         IF ((J.EQ.2) .AND. (IP1.EQ.0)) GO TO 25
            IRET = 1
            WRITE (MSGTXT,1100) J, IP1, IP2
            GO TO 990
C                                       Get type (and length) of col.
 25      STRLEN(1,J) = DATP(IP1,2) / 10
         COLTYP(1,J) = DATP(IP1,2) - STRLEN(1,J) * 10
         IF (IP2.GT.0) STRLEN(2,J) = DATP(IP2,2) / 10
         IF (IP2.GT.0) COLTYP(2,J) = DATP(IP2,2) - STRLEN(2,J) * 10
C                                       Only 1 col. for strings.
         IF ((COLTYP(1,J).EQ.3) .OR. (COLTYP(2,J).EQ.3) .OR.
     *      (COLTYP(1,J).EQ.7) .OR. (COLTYP(2,J).EQ.7)) THEN
            KEY(2,J) = 0
            IP2 = 0
            END IF
         IF ((KEYSUB(1,J).GE.1) .AND. (KEYSUB(1,J).LE.STRLEN(1,J)) .AND.
     *      (COLTYP(1,J).NE.3) .AND. (COLTYP(1,J).NE.7)) IPCOL(1,J) =
     *      IPCOL(1,J) + KEYSUB(1,J) - 1
         IF ((KEYSUB(2,J).GE.1) .AND. (KEYSUB(2,J).LE.STRLEN(2,J)) .AND.
     *      (COLTYP(2,J).NE.3) .AND. (COLTYP(2,J).NE.7) .AND.
     *      (IP2.NE.0)) IPCOL(2,J) = IPCOL(2,J) + KEYSUB(2,J) - 1
 30      CONTINUE
C                                       sort it
      WRITE (MSGTXT,1050) KEY(1,1)
      CALL MSGWRT (3)
      IF (KEY(2,2).EQ.0) THEN
         WRITE (MSGTXT,1051) KEY(1,2)
      ELSE IF (KEY(1,2).NE.KEY(2,2)) THEN
         WRITE (MSGTXT,1052) KEY(1,2), KEY(2,2)
      ELSE
         WRITE (MSGTXT,1053) KEY(1,2)
         END IF
      CALL MSGWRT (3)
C                                       sort input table
      IF ((TABUF1(43).NE.HDRSRT(1)) .OR. (TABUF1(44).NE.HDRSRT(2))) THEN
         CALL TABIO ('CLOS', 0, NROWO, IBUF1, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE 1ST TABLE'
            GO TO 990
            END IF
         CALL TABSRT (DISKI, CNOI, TTYPE, INVER1, INVER1, KEY, KEYSUB,
     *      FKEY, TABUF1, CATI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING INPUT TABLE 1'
            GO TO 990
            END IF
         CALL TABINI ('READ', TTYPE, DISKI, CNOI, INVER1, CATI, LUN1,
     *      NKEY, MREC, NCOL, DATP, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN 1ST INPUT TABLE'
            GO TO 990
            END IF
         NROW1 = TABUF1(5)
         END IF
C                                       Copy file 2 to scr
      CALL TABINI ('READ', TTYPE, DISKO, CNOO, INVER2, CATBLK, LUN2,
     *   NKEY, MREC, NCOL, DATP, TABUF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN 2ND'
         GO TO 990
         END IF
      NROW2 = TABUF2(5)
C                                       sort input table
      IF ((TABUF2(43).NE.HDRSRT(1)) .OR. (TABUF2(44).NE.HDRSRT(2))) THEN
         CALL TABIO ('CLOS', 0, NROWO, IBUF2, TABUF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE 2ND TABLE'
            GO TO 990
            END IF
         CALL TABSRT (DISKO, CNOO, TTYPE, INVER2, INVER2, KEY, KEYSUB,
     *      FKEY, TABUF2, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING INPUT TABLE 2'
            GO TO 990
            END IF
         CALL TABINI ('READ', TTYPE, DISKO, CNOO, INVER2, CATBLK, LUN2,
     *      NKEY, MREC, NCOL, DATP, TABUF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN 1ST INPUT TABLE'
            GO TO 990
            END IF
         NROW2 = TABUF2(5)
         END IF
C                                       create scr table
      CALL TABINI ('WRIT', TTYPE, DISKO, CNOO, OUTVER, CATBLK, LUNO,
     *   NKEY, MREC, NCOL, DATP, TABUFO, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUT SCR'
         GO TO 990
         END IF
C                                       col labels, units
      DO 100 IREC1 = 1,NCOL
         CALL TABIO ('READ', 3, IREC1, IBUF1, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ TYPE 3'
            GO TO 990
            END IF
         CALL TABIO ('WRIT', 3, IREC1, IBUF1, TABUFO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE TYPE 3'
            GO TO 990
            END IF
         CALL TABIO ('READ', 4, IREC1, IBUF1, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ TYPE 4'
            GO TO 990
            END IF
         CALL TABIO ('WRIT', 4, IREC1, IBUF1, TABUFO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE TYPE 4'
            GO TO 990
            END IF
 100     CONTINUE
C                                       keywords
      IF (NKEY.GT.0) THEN
         CALL TABKEY ('ALL ', KEYW, NKEY, TABUF1, KLOCS, DBUF1, KEYTYP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ KEYWORDS'
            GO TO 990
            END IF
         CALL TABKEY ('WRIT', KEYW, NKEY, TABUFO, KLOCS, DBUF1, KEYTYP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE KEYWORDS'
            GO TO 990
            END IF
         END IF
C                                       selection strings
      NSEL = TABUF1(61)
      DO 110 IREC1 = 1,NSEL
         CALL TABIO ('READ', 2, IREC1, IBUF1, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ TYPE 2'
            GO TO 990
            END IF
         CALL TABIO ('WRIT', 2, IREC1, IBUF1, TABUFO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE TYPE 2'
            GO TO 990
            END IF
 110     CONTINUE
C                                       data
      NROWO = 0
      IREC1 = 0
      IREC2 = 0
      READ1 = .TRUE.
      READ2 = .TRUE.
C                                       read loop
 200  IF ((IREC1.LT.NROW1) .OR. (IREC2.LT.NROW2)) THEN
         IF ((READ1) .AND. (IREC1.LT.NROW1)) THEN
            IREC1 = IREC1 + 1
            CALL TABIO ('READ', 0, IREC1, IBUF1, TABUF1, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ 1ST FILE DATA ROW'
               GO TO 990
            ELSE IF (IRET.LT.0) THEN
               GO TO 200
            ELSE
               READ1 = .FALSE.
               END IF
            END IF
         IF ((READ2) .AND. (IREC2.LT.NROW2)) THEN
            IREC2 = IREC2 + 1
            CALL TABIO ('READ', 0, IREC2, IBUF2, TABUF2, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ 2ND FILE DATA ROW'
               GO TO 990
            ELSE IF (IRET.LT.0) THEN
               GO TO 200
            ELSE
               READ2 = .FALSE.
               END IF
            END IF
C                                       compare
         IF ((IREC1.LE.NROW1) .AND. (IREC2.LE.NROW2)) THEN
            CALL GETNXT (NWORDS, KEY, FKEY, COLTYP, IPCOL, DBUF1, DBUF2,
     *         NEXT)
C                                       write from 1st file
            IF (NEXT.EQ.1) THEN
               NROWO = NROWO + 1
               CALL TABIO ('WRIT', 0, NROWO, IBUF1, TABUFO, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE DATA ROW'
                  GO TO 990
                  END IF
               READ1 = .TRUE.
               IF (IREC1.GE.NROW1) IREC1 = NROW1 + 1
C                                       write from 2nd file
            ELSE IF (NEXT.EQ.2) THEN
               NROWO = NROWO + 1
               CALL TABIO ('WRIT', 0, NROWO, IBUF2, TABUFO, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE DATA ROW'
                  GO TO 990
                  END IF
               READ2 = .TRUE.
               IF (IREC2.GE.NROW2) IREC2 = NROW2 + 1
C                                       if equal are rows identical?
            ELSE
               DO 210 J = 1,NWORDS
                  IF (IBUF1(J).NE.IBUF2(J)) GO TO 220
 210              CONTINUE
               IF (IREC1.LT.NROW1) THEN
                  READ1 = .TRUE.
               ELSE
                  READ2 = .TRUE.
                  END IF
               GO TO 200
C                                       differ write 1
 220           NROWO = NROWO + 1
               CALL TABIO ('WRIT', 0, NROWO, IBUF1, TABUFO, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE DATA ROW'
                  GO TO 990
                  END IF
               READ1 = .TRUE.
               IF (IREC1.GE.NROW1) IREC1 = NROW1 + 1
               END IF
         ELSE IF (IREC1.LE.NROW1) THEN
            NROWO = NROWO + 1
            CALL TABIO ('WRIT', 0, NROWO, IBUF1, TABUFO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE DATA ROW'
               GO TO 990
               END IF
            READ1 = .TRUE.
            IF (IREC1.GE.NROW1) IREC1 = NROW1 + 1
         ELSE IF (IREC2.LE.NROW2) THEN
            NROWO = NROWO + 1
            CALL TABIO ('WRIT', 0, NROWO, IBUF2, TABUFO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE DATA ROW'
               GO TO 990
               END IF
            READ2 = .TRUE.
            IF (IREC2.GE.NROW2) IREC2 = NROW2 + 1
            END IF
         GO TO 200
         END IF
C                                       close file 1
      CALL TABIO ('CLOS', 0, IREC1, IBUF1, TABUF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE 1ST TABLE'
         GO TO 990
         END IF
C                                       close file 2
      CALL TABIO ('CLOS', 0, IREC2, IBUF2, TABUF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE 2ND TABLE'
         GO TO 990
         END IF
C                                       close scr 1
      CALL TABIO ('CLOS', 0, NROWO, IBUF1, TABUFO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE OUTPUT TABLE'
         GO TO 990
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPPED: ERROR',I4,' DOING ',A)
 1050 FORMAT ('Main sort on column',I4)
 1051 FORMAT ('Secondary sort on column',I4)
 1052 FORMAT ('Secondary sort on baseline code columns',2I4)
 1053 FORMAT ('Secondary sort on baseline from column',I4,' array')
 1100 FORMAT ('TAPPED: COLUMN OUT OF RANGE KEY',I3,' =',2I5)
      END
      SUBROUTINE TAPPEH (IRET, SCRBUF)
C-----------------------------------------------------------------------
C   TAPPEH cleans up after TAPPE -- deleting scratch extension files and
C   (on error) any other newly created ones.  It then adds information
C   to the history file.
C   Inputs:
C      IRET     I      Error code from previous routines
C   Output:
C      SCRBUF   I(*)   Scratch
C-----------------------------------------------------------------------
      INTEGER   IRET, SCRBUF(*)
C
      CHARACTER HILINE*72, ATIME*8, ADATE*12, CHTMP*2, PHNAME*48
      INTEGER   I, EMIN, EMAX, JP, IVER, IERR, ID(3), IT(3), HLUN
      LOGICAL   T
      INCLUDE 'TAPPE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA T /.TRUE./
      DATA HLUN /27/
C-----------------------------------------------------------------------
C                                       extension file clean up
      EMAX = 0
      DO 10 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+I-1), CHTMP)
         IF (CHTMP.EQ.TTYPE) THEN
            JP = KIVER + I - 1
            EMAX = CATBLK(JP)
            GO TO 20
            END IF
 10      CONTINUE
 20   EMIN = MAXVER + 1
      IF (IRET.EQ.0) EMIN = MAX (EMIN, OUTVER+1)
C                                       delete the extras
      IF (EMAX.GE.EMIN) THEN
         CATBLK(JP) = EMIN - 1
         DO 30 IVER = EMIN,EMAX
            CALL ZPHFIL (TTYPE, DISKO, CNOO, IVER, PHNAME, IERR)
            CALL ZDESTR (DISKO, PHNAME, IERR)
            IF (IERR.GT.1) CATBLK(JP) = IVER
 30         CONTINUE
C                                       Update catalog
         CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', SCRBUF, IERR)
         END IF
C                                       Do history
      IF (IRET.NE.0) GO TO 999
C                                       Open file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISKO, CNOO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Version, date, time
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1110) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       input file
      CALL HENCO1 (TSKNAM, NAMEI, CLASSI, SEQI, DISKI, HLUN, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Input vers, number
      WRITE (MSGTXT,1111) NROW1, TTYPE, INVER1
      CALL MSGWRT (3)
      WRITE (HILINE,1112) TSKNAM, NROW1, TTYPE, INVER1
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCO1 (TSKNAM, NAMEO, CLASSO, SEQO, DISKO, HLUN, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (MSGTXT,1111) NROW2, TTYPE, INVER2
      CALL MSGWRT (3)
      WRITE (HILINE,1112) TSKNAM, NROW2, TTYPE, INVER2
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Output vers, number
      WRITE (MSGTXT,1113) NROWO, TTYPE, OUTVER
      CALL MSGWRT (3)
      WRITE (HILINE,1114) TSKNAM, NROWO, TTYPE, OUTVER
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 200
C
 200  CALL HICLOS (HLUN, T, SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR:',I7,' OPENING HISTORY FILE')
 1110 FORMAT (A6,'Release =''',A7,' ''  /********* run ',A12,2X,A8)
 1111 FORMAT ('Read ',I10,' rows from ',A2,' file version',I5)
 1112 FORMAT (A6,'/ Read ',I10,' rows from ',A2,' file version',I5)
 1113 FORMAT ('Wrote',I10,' rows to ',A2,' file version',I5)
 1114 FORMAT (A6,'/ Wrote',I10,' rows to ',A2,' file version',I5)
      END
      SUBROUTINE GETNXT (NWORDS, KEY, FKEY, COLTYP, IPCOL, DBUF1, DBUF2,
     *   NEXT)
C-----------------------------------------------------------------------
C   GETNXT gets the sort keys from 2 data sets and compares the
C   values telling the calling task whether 1 or 2 is next or they are
C   equal.
C   Inputs:
C      NWORDS   I        size of DBUF1 and 2
C      KEY      I(2,2)   Keys
C      FKEY     R(2,2)   Multipliers
C      COLTYP   I(2,2)   Column type
C      IPCOL    I(2,2)   column pointers (includes subscripts)
C      DBUF1    R(*)     first file table row
C      DBUF2    R(*)     second file table row
C   Outputs:
C      NEXT     I        1, 2, or 0 => equal
C-----------------------------------------------------------------------
      INTEGER   NWORDS, KEY(2,2), COLTYP(2,2), IPCOL(2,2), NEXT
      REAL      FKEY(2,2), DBUF1(*), DBUF2(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXREC
      PARAMETER (MXREC = XBPRSZ+2)
C
      INTEGER   JKEY, NOCOL, ICOL, IP, KKEY, LKEY, FFKEY, NCHR, INDEX,
     *   ICHA, IICHR, RECI1(MXREC), RECI2(MXREC)
      REAL      RECR1(MXREC), RECR2(MXREC)
      HOLLERITH RECH1(MXREC), RECH2(MXREC)
      DOUBLE PRECISION RECD1(MXREC/2), RECD2(MXREC/2), XVAL1(2,2),
     *   XVAL2(2,2)
      CHARACTER CHR1*32, CHR2*32
      EQUIVALENCE (RECI1, RECR1, RECH1, RECD1)
      EQUIVALENCE (RECI2, RECR2, RECH2, RECD2)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL COPY (NWORDS, DBUF1, RECI1)
      CALL COPY (NWORDS, DBUF2, RECI2)
      CALL DFILL (4, 0.0D0, XVAL1)
      CALL DFILL (4, 0.0D0, XVAL2)
      DO 100 JKEY = 1,2
         IF (KEY(1,JKEY).EQ.0) GO TO 100
         NOCOL = 2
         IF (KEY(2,JKEY).EQ.0) NOCOL = 1
         DO 80 ICOL = 1,NOCOL
C                                       Select key.
            IP = IPCOL(ICOL,JKEY)
            KKEY = COLTYP(ICOL,JKEY)
            LKEY = KEY(ICOL,JKEY)
            FFKEY = FKEY(ICOL,JKEY)
C                                       Double precision (1)
            IF (KKEY.EQ.1) THEN
               IF (LKEY.GT.0) THEN
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - RECD1(IP)*FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - RECD2(IP)*FFKEY
               ELSE
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - ABS (RECD1(IP))
     *               * FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - ABS (RECD2(IP))
     *               * FFKEY
                  END IF
C                                       Single precision (2)
            ELSE IF (KKEY.EQ.2) THEN
               IF (LKEY.GT.0) THEN
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - RECR1(IP)*FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - RECR2(IP)*FFKEY
               ELSE
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - ABS (RECR1(IP))
     *               * FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - ABS (RECR2(IP))
     *               * FFKEY
                  END IF
C                                       Char. string. (3)
            ELSE IF (KKEY.EQ.3) THEN
               NCHR = FKEY(2,JKEY) + 0.5
               INDEX = FKEY(1,JKEY) + 0.5
               ICHA = ICHAR ('A')
               CALL H2CHR (NCHR, INDEX, RECH1(IP), CHR1)
               CALL H2CHR (NCHR, INDEX, RECH2(IP), CHR2)
               DO 40 IICHR = 1,NCHR
C                                       Use char-"A" * 64**(NCHR-IICHR)
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) -
     *               (ICHAR (CHR1(IICHR:IICHR)) - ICHA) *
     *               64 ** (NCHR-IICHR)
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) -
     *               (ICHAR (CHR2(IICHR:IICHR)) - ICHA) *
     *               64 ** (NCHR-IICHR)
 40               CONTINUE
C                                       Integer (4)
            ELSE IF (KKEY.EQ.4) THEN
               IF (LKEY.GT.0) THEN
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - RECI1(IP)*FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - RECI2(IP)*FFKEY
               ELSE
                  XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - ABS (RECI1(IP))
     *               * FFKEY
                  XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - ABS (RECI2(IP))
     *               * FFKEY
                  END IF
C                                       Logical (use I)   (5)
            ELSE IF (KKEY.EQ.5) THEN
               XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - RECI1(IP)*FFKEY
               XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - RECI2(IP)*FFKEY
C                                       Short integer (Illegal) (6)
            ELSE IF (KKEY.EQ.5) THEN
               MSGTXT = 'SHORT INTEGERS NOW ILLEGAL'
C                                       Bit array (7)
C                                       This doesn't make  much sense
C                                       use I   array.
            ELSE IF (KKEY.EQ.5) THEN
               XVAL1(ICOL,JKEY) = XVAL1(ICOL,JKEY) - RECI1(IP)*FFKEY
               XVAL2(ICOL,JKEY) = XVAL2(ICOL,JKEY) - RECI2(IP)*FFKEY
               END IF
 80         CONTINUE
 100     CONTINUE
C
      XVAL1(1,1) = XVAL1(1,1) + XVAL1(2,1)
      XVAL1(1,2) = XVAL1(1,2) + XVAL1(2,2)
      XVAL2(1,1) = XVAL2(1,1) + XVAL2(2,1)
      XVAL2(1,2) = XVAL2(1,2) + XVAL2(2,2)
      NEXT = 0
      IF (XVAL1(1,1).GT.XVAL2(1,1)) THEN
         NEXT = 1
      ELSE IF (XVAL1(1,1).LT.XVAL2(1,1)) THEN
         NEXT = 2
      ELSE IF (XVAL1(1,2).GT.XVAL2(1,2)) THEN
         NEXT = 1
      ELSE IF (XVAL1(1,2).LT.XVAL2(1,2)) THEN
         NEXT = 2
         END IF
C
 999  RETURN
      END
