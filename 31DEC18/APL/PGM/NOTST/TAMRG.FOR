LOCAL INCLUDE 'TAMRG.INC'
C                                       Local include for TAMRG
      INTEGER   CATBLK(256), USERID, SEQ, INVER, OUTVER, SCVER(2),
     *   CNO, DISK, EQUKOL(10), SUMKOL(10), MAXVER, INPCMP, OUTCMP
      HOLLERITH XNAME(3), XCLASS(2), XXTYPE
      CHARACTER NAME*12, CLASS*6, XTYPE*2
      REAL      XSEQ, XDISK, XVER, X2VER, APARM(10), BPARM(10),
     *   CPARM(10), DPARM(10), BADD(10)
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XXTYPE, XVER, X2VER,
     *   APARM, BPARM, CPARM, DPARM, BADD, INPCMP, OUTCMP, USERID, SEQ,
     *   INVER, OUTVER, SCVER, DISK, CNO, EQUKOL, SUMKOL, MAXVER
      COMMON /CHRCOM/ NAME, CLASS, XTYPE
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM TAMRG
C-----------------------------------------------------------------------
C! Merges entries in a table as specified by user
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2009, 2011
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
C   Task TAMRG sorts AIPS tables to bring all rows at the same control
C   values together, then it sums them, and finally it resorts the file
C   into the original order.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name.
C      INCLASS        CLASS         File class.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volume on which file resides.
C      INEXT          INEXT         Extension file type
C      INVER          INVER         Input file version number
C      IN2VER         OUTVER        Output file version number
C      APARM                        Initial sort: Keys & weights
C                                     1,2=> col. & wt pri. key 1.
C                                     3,4=> col. & wt sec. key 1.
C                                     5,6=> col. & wt pri. key 2.
C                                     7,8=> col. & wt sec. key 2.
C                                     APARM(1) = 0 => no sort.
C      BPARM                        Columns which must be equal
C                                     to merge two rows.
C      CPARM                        Allowed error in the equality
C                                     test by column.
C      DPARM                        Columns to sum. 0 => none.
C      BADDISK        IBADD(10)     Disks to avoid for scratch.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, PHNAME*48
      DOUBLE PRECISION CATD(128)
      INTEGER   KEY(2,4), IRET, TABUFF(512), IERR, KEYSUB(2,2)
      REAL      BUFFER(512), FKEY(2,4), CATR(256)
      INCLUDE 'TAMRG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Set file info, keys, versions
      CALL TAMRGI (TYPE, KEY, FKEY, TABUFF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Sort: to scratch (1)
      IF (KEY(1,1).NE.0) THEN
         WRITE (MSGTXT,1000) SCVER(1)
         CALL MSGWRT (2)
         CALL TABSRT (DISK, CNO, TYPE, INVER, SCVER(1), KEY, KEYSUB,
     *      FKEY, TABUFF, CATBLK, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Merge: scratch(2)
      WRITE (MSGTXT,1010) SCVER(2)
      IF (KEY(1,3).EQ.0) WRITE (MSGTXT,1011) SCVER(2)
      CALL MSGWRT (2)
      CALL TABMRG (DISK, CNO, TYPE, SCVER(1), SCVER(2), EQUKOL, SUMKOL,
     *   CPARM, BUFFER, TABUFF, CATBLK, OUTCMP, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Sort to output
      IF (KEY(1,3).NE.0) THEN
         WRITE (MSGTXT,1020) OUTVER
         CALL MSGWRT (2)
         CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, PHNAME, IERR)
         CALL ZDESTR (DISK, PHNAME, IERR)
         CALL TABSRT (DISK, CNO, TYPE, SCVER(2), OUTVER, KEY(1,3),
     *      KEYSUB, FKEY(1,3), TABUFF, CATBLK, IRET)
         END IF
C                                       History and clean up
 900  CALL TAMRGH (IRET, TYPE, KEY, FKEY, TABUFF)
C                                       Write end message
 990  CALL DIE (IRET, TABUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Start sort to scratch version',I4)
 1010 FORMAT ('Start merge to scratch version',I4)
 1011 FORMAT ('Start merge to output version',I4)
 1020 FORMAT ('Start sort to output version',I4)
      END
      SUBROUTINE TAMRGI (TYPE, KEY, FKEY, TABUFF, IRET)
C-----------------------------------------------------------------------
C   TAMRGI performs initialization for TAMRG
C   Output: TYPE    C*2         Extension file type
C           KEY     I(2,4)      In(1,2), out(3,4) sort cols
C           FKEY    R(2,4)      Weights for sort cols
C           TABUFF  I(*)        Scratch
C           IRET    I           Error code: 0 -> okay
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, PRGM*6, PTYPE*2
      INTEGER   KEY(2,4), IRET, TABUFF(*)
      REAL      FKEY(2,4)
      DOUBLE PRECISION    CATD(128)
      INTEGER   NPARM,IROUND, LUN, FIND, JCOL1, IERR, I, NKEY, NCOL,
     *   NREC, DATP(128,2), JCOL2, IRNO
      REAL      CATR(256)
      INCLUDE 'TAMRG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA PRGM  /'TAMRG '/
      DATA LUN /16/
C-----------------------------------------------------------------------
C                                       Init I/O, parameters
      NPARM = 60
      CALL SETUP (PRGM, NPARM, XNAME, TABUFF, IRET)
      IF (IRET.NE.0) GO TO 990
      USERID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      INVER = IROUND (XVER)
      OUTVER = IROUND (X2VER)
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (2, 1, XXTYPE, XTYPE)
      IF (XTYPE.EQ.'  ') XTYPE = 'TA'
      TYPE(1:2) = XTYPE(1:2)
C                                       Open file and get CATBLK.
      PTYPE = '  '
      CALL MAPOPN ('WRIT', DISK, NAME, CLASS, SEQ, PTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, TABUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 990
C                                       Mark in /CFILES/
 10   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Close file, leave marked.
      CALL ZCLOSE (LUN, FIND, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         CALL MSGWRT (8)
         GO TO 990
C                                       BADDISK
 20   DO 25 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 25      CONTINUE
C                                       find version numbers
      CALL FNDEXT (TYPE, CATBLK, MAXVER)
      IF ((INVER.LE.0) .OR. (INVER.GT.MAXVER)) INVER = MAXVER
      IF ((OUTVER.LE.0) .OR. (OUTVER.GT.MAXVER)) OUTVER = MAXVER + 1
      SCVER(1) = MAXVER + 1
      SCVER(2) = MAXVER + 2
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', TYPE, DISK, CNO, INVER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1025) IERR, TYPE, INVER
         GO TO 990
C                                       Find columns
 30   CALL FILL (10, 0, EQUKOL)
      CALL FILL (10, 0, SUMKOL)
C                                       insort
      KEY(1,1) = IROUND (APARM(1))
      KEY(2,1) = IROUND (APARM(3))
      KEY(1,2) = IROUND (APARM(5))
      KEY(2,2) = IROUND (APARM(7))
C                                       sort weights: not sum cols
      FKEY(1,1) = APARM(2)
      FKEY(2,1) = APARM(4)
      FKEY(1,2) = APARM(6)
      FKEY(2,2) = APARM(8)
      IF (FKEY(1,1).EQ.0.0) FKEY(1,1) = 1.0
      IF (FKEY(1,2).EQ.0.0) FKEY(1,2) = 1.0
      IF (FKEY(2,1).EQ.0.0) FKEY(2,1) = 1.0
      IF (FKEY(2,2).EQ.0.0) FKEY(2,2) = 1.0
      I = ABS(KEY(1,1))
      IF ((I.GT.0) .AND. (I.LE.NCOL)) GO TO 40
         KEY(1,1) = 0
         SCVER(2) = MAXVER + 1
         SCVER(1) = INVER
C                                       outsort
 40   KEY(1,3) = ABS (TABUFF(43))
      KEY(2,3) = 0
      KEY(1,4) = ABS (TABUFF(44))
      KEY(2,4) = 0
      FKEY(1,3) = 1.0
      FKEY(2,3) = 1.0
      FKEY(1,4) = 1.0
      FKEY(2,4) = 1.0
      IF (TABUFF(43).LT.0) FKEY(1,3) = -1.0
      IF (TABUFF(44).LT.0) FKEY(1,4) = -1.0
      IF (KEY(1,3).GT.256) KEY(1,3) = 256 - KEY(1,3)
      IF (KEY(1,4).GT.256) KEY(1,4) = 256 - KEY(1,4)
      I = ABS (KEY(1,3))
      IF ((I.GT.0) .AND. (I.LE.NCOL)) GO TO 45
         KEY(1,3) = 0
         SCVER(2) = OUTVER
C                                       Merge columns
 45   JCOL1 = 0
      DO 50 I = 1,10
         JCOL2 = IROUND (BPARM(I))
         IF ((JCOL2.LE.0) .OR. (JCOL2.GT.NCOL)) GO TO 55
            JCOL1 = I
            EQUKOL(I) = JCOL2
            IF (CPARM(I).LT.0.0) CPARM(I) = 0.0
 50      CONTINUE
 55   IF (JCOL1.GT.0) GO TO 60
         WRITE (MSGTXT,1055)
         IRET = 4
         GO TO 990
C                                       Sum columns
 60   DO 65 I = 1,10
         JCOL2 = IROUND (DPARM(I))
         IF ((JCOL2.LE.0) .OR. (JCOL2.GT.NCOL)) GO TO 70
            SUMKOL(I) = JCOL2
 65      CONTINUE
C                                       Number input components
 70   INPCMP = TABUFF(5)
C                                       Close the table file
      IRNO = 0
      CALL TABIO ('CLOS', 0, IRNO, DATP, TABUFF, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,' OPENING MAP FILE')
 1010 FORMAT ('ERROR:',I7,' CLOSING MAP FILE')
 1025 FORMAT ('ERROR:',I7,' OPENING ',A2,' FILE VERSION',I3)
 1055 FORMAT ('ERROR: SOME COLUMNS MUST BE REQUIRED TO BE EQUAL')
      END
      SUBROUTINE TAMRGH (IRET, TYPE, KEY, FKEY, TABUFF)
C-----------------------------------------------------------------------
C   TAMRGH cleans up after TAMRG -- deleting scratch extension files and
C   (on error) any other newly created ones.  It then adds information
C   to the history file.
C   Inputs: IRET    I       Error code from previous routines
C   Output: TABUFF  I(*)    Scratch
C-----------------------------------------------------------------------
      CHARACTER   HILINE*72, ATIME*8, ADATE*12, CHTMP*2,
     *   PHNAME*48, TYPE*2, SPEEDS(4)*4, DIR(4)*4
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128)
      INTEGER   IRET, TABUFF(*), I, EMIN, EMAX, JP, IVER, IERR,
     *   ID(3), IT(3), HLUN, KEY(2,4)
      REAL      CATR(256), FKEY(2,4)
      LOGICAL   T
      INCLUDE 'TAMRG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA T /.TRUE./
      DATA HLUN /27/
      DATA SPEEDS /'slow','fast','slow','fast'/
      DATA DIR /' In ',' In ','Out ','Out '/
C-----------------------------------------------------------------------
C                                       extension file clean up
      CALL FXHDEX (CATBLK)
      DO 10 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+I-1), CHTMP)
         IF (CHTMP.NE.TYPE) GO TO 10
            JP = KIVER + I - 1
            IF (CATBLK(JP).LE.MAXVER) GO TO 10
               EMAX = CATBLK(JP)
               GO TO 20
 10      CONTINUE
      GO TO 100
 20   EMIN = MAXVER + 1
      IF (IRET.EQ.0) EMIN = MAX (EMIN, OUTVER+1)
C                                       delete the extras
      IF (EMIN.GT.EMAX) GO TO 100
         CATBLK(JP) = EMIN - 1
         DO 30 IVER = EMIN,EMAX
            CALL ZPHFIL (TYPE, DISK, CNO, IVER, PHNAME, IERR)
            CALL ZDESTR (DISK, PHNAME, IERR)
            IF (IERR.GT.1) CATBLK(JP) = IVER
 30         CONTINUE
C                                       Update catalog
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', TABUFF, IERR)
C                                       Do history
 100  IF (IRET.NE.0) GO TO 999
C                                       Open file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISK, CNO, TABUFF, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Version, date, time
 110  CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1110) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (HLUN, HILINE, TABUFF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Input vers, number
      WRITE (MSGTXT,1111) INPCMP, XTYPE, INVER
      CALL MSGWRT (3)
      WRITE (HILINE,1112) TSKNAM, INPCMP, XTYPE, INVER
      CALL HIADD (HLUN, HILINE, TABUFF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Output vers, number
      WRITE (MSGTXT,1113) OUTCMP, XTYPE, OUTVER
      CALL MSGWRT (3)
      WRITE (HILINE,1114) TSKNAM, OUTCMP, XTYPE, OUTVER
      CALL HIADD (HLUN, HILINE, TABUFF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Sorts
 120  DO 125 I = 1,4
         IF (KEY(1,I).EQ.0) GO TO 125
         IF (((I.EQ.2) .OR. (I.EQ.4)) .AND. (KEY(1,I-1).EQ.0)) GO TO 125
            JP = ABS (KEY(1,I))
            IF (KEY(1,I).GT.0) WRITE (HILINE,1120) TSKNAM, DIR(I),
     *         SPEEDS(I), FKEY(1,I), JP
            IF (KEY(1,I).LT.0) WRITE (HILINE,1121) TSKNAM, DIR(I),
     *         SPEEDS(I), FKEY(1,I), JP
            CALL HIADD (HLUN, HILINE, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 200
            IF (KEY(2,I).EQ.0) GO TO 125
               JP = ABS (KEY(2,I))
               IF (KEY(2,I).GT.0) WRITE (HILINE,1122) TSKNAM,
     *            FKEY(2,I), JP
               IF (KEY(2,I).LT.0) WRITE (HILINE,1123) TSKNAM,
     *            FKEY(2,I), JP
               CALL HIADD (HLUN, HILINE, TABUFF, IERR)
               IF (IERR.NE.0) GO TO 200
 125     CONTINUE
C                                       Merge parameters: equals
 130  DO 135 I = 1,10
         IF (EQUKOL(I).LE.0) GO TO 140
            WRITE (HILINE,1130) TSKNAM, EQUKOL(I), CPARM(I)
            CALL HIADD (HLUN, HILINE, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 200
 135     CONTINUE
C                                       Merge parameters: sums
 140  DO 145 I = 1,10
         IF (SUMKOL(I).LE.0) GO TO 200
            WRITE (HILINE,1140) TSKNAM, SUMKOL(I)
            CALL HIADD (HLUN, HILINE, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 200
 145     CONTINUE
C
 200  CALL HICLOS (HLUN, T, TABUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR:',I7,' OPENING HISTORY FILE')
 1110 FORMAT (A6,'Release =''',A7,' ''  /********* run ',A12,2X,A8)
 1111 FORMAT ('Read ',I10,' rows from ',A2,' file version',I3)
 1112 FORMAT (A6,'/Read ',I10,' rows from ',A2,' file version',
     *   I3)
 1113 FORMAT ('Wrote',I10,' rows to ',A2,' file version',I3)
 1114 FORMAT (A6,'/Wrote',I10,' rows to ',A2,' file version',
     *   I3)
 1120 FORMAT (A6,'/',A3,'put sort: ',A4,'er ',F9.3,' Val(col.',I3,')')
 1121 FORMAT (A6,'/',A3,'put sort: ',A4,'er ',F9.3,' Absval(col.',I3,
     *   ')')
 1122 FORMAT (A6,'/',15X,'plus ',F9.3,' Val(col.',I3,')')
 1123 FORMAT (A6,'/',15X,'plus ',F9.3,' Absval(col.',I3,')')
 1130 FORMAT (A6,'/Val(col.',I3,') equal within',1PE12.4,' to merge')
 1140 FORMAT (A6,'/Val(col.',I3,') summed when rows merged')
      END
