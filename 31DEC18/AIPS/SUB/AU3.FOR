      SUBROUTINE AU3 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to display contents of catalogs and headers: CATA, IMHE ...
C# POPS-appl Catalog Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2008-2009, 2015
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
C   Verb subroutine to list the catalog directory on a given disk or
C   disks; to list the detailed contents of a catalog block; or to
C   delete a catalog block--at the same time destoying the file and any
C   associated files
C   Input:
C      BRANCH 1 - CATALOG: list catalog directory
C             2 - MCAT: catalog for MAps ignore INNAME,INCLASS,
C                 INSEQ and write to terminal only
C             3 - IMHEADER: list entire catalog entry
C             4 - ZAP: destroy map file, extension files, cat entry
C             5 - UCAT: catalog for UVs ignore INNAME,INCLASS,
C                 INSEQ and write to terminal only
C             6 - QHEADER: shorter than IMHEADER and positions are for
C                 the numeric center of the image
C             7 - PCAT: quick catalog listing all types, names in
C                 range 1 - SLOT or SLOT to infinty.
C             8 - SIZEFILE - return file size, IMAGR work file size
C             9 - FILEZAP - delete external file (immediate argument)
C            10 - REVERSN - find all version on disk
C   Adverbs from common:
C      INNAME  - name of map; on CATALOG only maps with this name are
C                listed; blanks mean "any name"
C      INCLASS - select on this TYPE; blanks mean "any"
C      INSEQ   - select on this sequence number; 0 means any
C      INTYPE  - select on this physical type; blanks mean "any"
C      INDISK  - disk volume to list; 0 means any but is not allowed on
C                destroy
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NONUMB
      PARAMETER (NONUMB=21)
C
      CHARACTER STAT*4, XXTYPE*2, PRGNAM*6, LOCNAM*12, LOCCLS*6, CDUM*1,
     *   KEYWOD(50)*8, KEYCHR*8, PHNAME*48, HDTYPE*2, NONAME(NONUMB)*2,
     *   ALPH*26, IMTYPE*2
      INTEGER   IUSER, BDISK, EDISK, IVOL, IERR, SCAT, BUFF(256), I,
     *   NUMKEY, BUFF2(256), LOCSEQ, POTERR, QUICK, LOCS(50), J, IDUM,
     *   KEYTS(50), IVALUE(100), ISIZE, NCHAV, BIF, EIF, VER, JTRIM,
     *   SYTYPE, NSIZE, IOFF, COUNT, LUN, FIND, MSGSAV, IC, JC, HPOS,
     *   MISS
      REAL      VALUE(100), RDUM, RSIZE
      LOGICAL   LVALUE(100), DOIT
      DOUBLE PRECISION BSC, BZE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DCON.INC'
      COMMON /AIPSCR/ BUFF, BUFF2
      EQUIVALENCE (IVALUE, LVALUE, VALUE)
      DATA PRGNAM /'AU3 '/
      DATA ALPH /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA NONAME /'AC','BA','BQ','HE','IC','ID','IN','ME','SP','TP',
     *   'TD','GR','PW','MT','TC','TK','TV', 'CB', 'MA', 'UV', 'SC'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
      MSGSAV = MSGSUP
C                                        set common values
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
      IF (ERRNUM.NE.0) GO TO 990
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
      IF (ERRNUM.NE.0) GO TO 990
      IUSER = NLUSER
C                                        default disk = all disks
      IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) THEN
         BDISK = 1
         EDISK = NVOL
         IVOL = 0
      ELSE
         BDISK = IVOL
         EDISK = BDISK
         END IF
C                                        go to operation
      GO TO (100, 100, 300, 400, 100, 600, 100, 800, 900, 910), BRANCH
C-----------------------------------------------------------------------
C                                        CATALOG  &  CAT
C                                        list catalog entries
C-----------------------------------------------------------------------
 100  QUICK = 1
      IF (BRANCH.EQ.1) QUICK = -1
      CALL ADVERB ('DOALPHA', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      IF (RDUM.GT.0.0) QUICK = 2 * QUICK
      I = 0
      IF (BRANCH.EQ.7) THEN
         CALL ADVERB ('SLOT', 'I', 1, 0, I, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 990
         END IF
      XXTYPE = ' '
      IF (BRANCH.EQ.1) THEN
         CALL ADVERB ('INTYPE', 'C', 1,  2, IDUM, RDUM, XXTYPE)
         IF (ERRNUM.NE.0) GO TO 990
         END IF
      IF (BRANCH.EQ.2) XXTYPE = 'MA'
      IF (BRANCH.EQ.5) XXTYPE = 'UV'
      CALL CATLST (IVOL, LOCNAM, LOCCLS, LOCSEQ, XXTYPE, IUSER, QUICK,
     *   I, BUFF, BUFF2, IERR)
      GO TO 999
C-----------------------------------------------------------------------
C                                       IMHEADER
C                                       list file header
C-----------------------------------------------------------------------
 300  SCAT = 1
      POTERR = 45
      XXTYPE = ' '
      CALL CATDIR ('SRNH', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ, XXTYPE,
     *    IUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       one found: read catlg block
      CALL CATIO ('READ', IVOL, SCAT, CATBLK, 'REST', BUFF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) GO TO 990
C                                       read keywords on scaling
      NUMKEY = 2
      KEYWOD(1) = 'ISCALE'
      KEYWOD(2) = 'IZERO'
      CALL CATKEY ('REED', IVOL, SCAT, KEYWOD, NUMKEY, LOCS, VALUE,
     *   KEYTS, BUFF, IERR)
      BSC = 1.0D0
      BZE = 0.0D0
      IF ((IERR.LE.0) .OR. (IERR.GT.20)) THEN
         IF (LOCS(1).GT.0) CALL RCOPY (NWDPDP, VALUE(LOCS(1)), BSC)
         IF (LOCS(2).GT.0) CALL RCOPY (NWDPDP, VALUE(LOCS(2)), BZE)
         END IF
C                                       finally list the header
      CALL LSTHDR (CATBLK, CATH, CATR, CATD, BSC, BZE)
C                                       All other keywords
      NUMKEY = 50
      CALL CATKEY ('ALL ', IVOL, SCAT, KEYWOD, NUMKEY, LOCS, VALUE,
     *   KEYTS, BUFF, IERR)
      IF ((IERR.NE.0) .OR. (NUMKEY.LE.0)) GO TO 999
      DO 320 I = 1,NUMKEY
         IF (KEYWOD(I).EQ.'ISCALE') GO TO 320
         IF (KEYWOD(I).EQ.'IZERO') GO TO 320
         J = LOCS(I)
         IF (J.LE.0) GO TO 320
         IF (KEYTS(I).EQ.1) THEN
            CALL RCOPY (NWDPDP, VALUE(J), BSC)
            WRITE (MSGTXT,1300) KEYWOD(I), BSC
         ELSE IF (KEYTS(I).EQ.2) THEN
            WRITE (MSGTXT,1301) KEYWOD(I), VALUE(J)
         ELSE IF (KEYTS(I).EQ.3) THEN
            CALL H2CHR (8, 1, VALUE(J), KEYCHR)
            WRITE (MSGTXT,1302) KEYWOD(I), KEYCHR
         ELSE IF (KEYTS(I).EQ.4) THEN
            WRITE (MSGTXT,1303) KEYWOD(I), IVALUE(J)
         ELSE IF (KEYTS(I).EQ.5) THEN
            WRITE (MSGTXT,1304) KEYWOD(I), LVALUE(J)
         ELSE
            WRITE (MSGTXT,1305) KEYWOD(I), KEYTS(I)
            END IF
         CALL MSGWRT (2)
 320     CONTINUE
      GO TO 999
C-----------------------------------------------------------------------
C                                       ZAP
C                                       delete file/extensions
C-----------------------------------------------------------------------
 400  POTERR = 46
C                                       error: disk required
      CALL ADVERB ('INTYPE', 'C', 1,  2, IDUM, RDUM, XXTYPE)
      IF (ERRNUM.NE.0) GO TO 990
      IF ((BDISK.EQ.EDISK) .AND. (XXTYPE.NE.' ')) GO TO 405
         WRITE (MSGTXT,1400) IVOL, XXTYPE
         CALL MSGWRT (7)
         GO TO 990
C                                       error: name required
 405  IF (LOCNAM.NE.' ') GO TO 410
         WRITE (MSGTXT,1405) LOCNAM
         CALL MSGWRT (7)
         GO TO 990
C                                       locate file
 410  SCAT = 1
      IF ((IUSER.LE.0) .OR. (IUSER.EQ.32000)) IUSER = NLUSER
      CALL CATDIR ('SRNH', BDISK, SCAT, LOCNAM, LOCCLS, LOCSEQ, XXTYPE,
     *   IUSER, STAT, BUFF, IERR)
      IF (IERR.EQ.0) GO TO 415
         POTERR = 45
         GO TO 990
C                                       Do destroy
 415  CALL MDESTR (BDISK, SCAT, CATBLK, BUFF, I, IERR)
      IF (IERR.EQ.0) GO TO 999
      GO TO 990
C-----------------------------------------------------------------------
C                                        QHEADER
C                                        short list: file header, center
C-----------------------------------------------------------------------
 600  SCAT = 1
      POTERR = 45
      XXTYPE = ' '
      CALL CATDIR ('SRNH', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ, XXTYPE,
     *    IUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        one found: read, list
      CALL CATIO ('READ', IVOL, SCAT, CATBLK, 'REST', BUFF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) GO TO 990
      CALL KWIKHD
      GO TO 999
C-----------------------------------------------------------------------
C                                       SIZEFILE
C                                       file size return
C-----------------------------------------------------------------------
 800  SCAT = 1
      POTERR = 45
      XXTYPE = ' '
      IUSER = NLUSER
      CALL CATDIR ('SRCH', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ, XXTYPE,
     *    IUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       one found: read catlg block
      CALL CATIO ('READ', IVOL, SCAT, CATBLK, 'REST', BUFF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) GO TO 990
C                                       full size
      VER = 1
      CALL ZPHFIL (XXTYPE, IVOL, SCAT, VER, PHNAME, IERR)
      CALL ZEXIST (IVOL, PHNAME, ISIZE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1800) IERR
         IF (IERR.EQ.1) MSGTXT = 'FILE DOES NOT EXIST'
         CALL MSGWRT (8)
         POTERR = 101
         GO TO 990
         END IF
      RSIZE = ISIZE / 1024.0
      WRITE (MSGTXT,1810) RSIZE
      CALL MSGWRT (2)
      IF (XXTYPE.EQ.'UV') THEN
         CALL ADVERB ('BIF', 'I', 1, 0, BIF, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 990
         CALL ADVERB ('EIF', 'I', 1, 0, EIF, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 990
         CALL ADVERB ('NCHAV', 'I', 1, 0, NCHAV, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 990
C                                       user must ask
         DOIT = (BIF.GT.0) .OR. (EIF.GT.0) .OR. (NCHAV.GT.0)
C                                       header must be okay
         IF (DOIT) CALL UVPGET (IERR)
         IF (IERR.NE.0) DOIT = .FALSE.
C                                       not multi-source
         IF ((DOIT) .AND. (ILOCSU.GE.0)) DOIT = .FALSE.
         IF (DOIT) THEN
            BIF = MAX (BIF, 1)
            IF (JLOCIF.LT.0) THEN
               EIF = 1
            ELSE
               BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
               IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
               EIF = MIN (EIF, CATBLK(KINAX+JLOCIF))
               END IF
            NCHAV = MAX (1, MIN (NCHAV, CATBLK(KINAX+JLOCF)))
            IF (CATBLK(KINAX).EQ.1) NRPARM = NRPARM - 2
            NCHAV = 3 * NCHAV * (EIF - BIF + 1) + NRPARM
            RSIZE = NVIS / (1024.0 * 1024.0)
            RSIZE = 4.0 * RSIZE * NCHAV
            WRITE (MSGTXT,1815) RSIZE
            CALL MSGWRT (2)
         ELSE
            MSGTXT = 'YOU MUST ASK FOR THE IMAGR WORKFILE WITH ADVERBS'
            IF (ILOCSU.GE.0) MSGTXT = 'IMAGR WORKFILE SIZE ONLY FOR'
     *         // ' SINGLE-SOURCE FILES'
            CALL MSGWRT (7)
            END IF
         END IF
      CALL ADVRBS ('FSIZE', 'R', 1, 0, I, RSIZE, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      GO TO 999
C-----------------------------------------------------------------------
C                                       FILEZAP
C                                       delete external file
C-----------------------------------------------------------------------
 900  POTERR = 7
      IF (SP.LT.4) GO TO 990
      POTERR = 8
      IF (STACK(SP).NE.2) GO TO 990
      NSIZE = STACK(SP-2)
      SYTYPE = STACK(SP-3)
      IF (((SYTYPE.LT.7) .OR. (SYTYPE.GT.9)) .AND. (SYTYPE.NE.14))
     *   GO TO 990
      IOFF = 1
      IF (SYTYPE.EQ.9) IOFF = NSIZE / 1024
      COUNT = NSIZE
      IF (SYTYPE.EQ.7) COUNT = K(NSIZE+3)
      IF (SYTYPE.EQ.9) COUNT = MOD (NSIZE, 1024) - IOFF + 1
      IF (COUNT.LE.0) GO TO 990
      CALL H2CHR (COUNT, IOFF, CH(STACK(SP-1)), PHNAME)
      SP = SP - 4
      J = JTRIM (PHNAME)
      CALL ADVERB ('DOCONFRM', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      IF (RDUM.GT.0.0) THEN
         IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) THEN
            MSGTXT = 'CONFIRMATION ALLOWED ONLY IN INTERACTIVE MODE'
            CALL MSGWRT (8)
            POTERR = 53
            IF (IUNIT.EQ.3) POTERR = 60
            GO TO 990
            END IF
         WRITE (MSGTXT,1900) PHNAME(:J)
         CALL MSGWRT (2)
         CALL CONFRM (IERR)
         IF (IERR.EQ.1) GO TO 999
         IF (IERR.GT.1) GO TO 990
         END IF
      LUN = 3
C                                       does it exist
      MSGSUP = 32000
      CALL ZTXOPN ('QRED', LUN, FIND, PHNAME, .TRUE., IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.5) THEN
         MSGTXT = '''' // PHNAME(:J) // '''  DOES NOT EXIST'
         CALL MSGWRT (7)
         RDUM = 1.0
      ELSE
         IF (IERR.EQ.0) THEN
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXZAP (LUN, PHNAME, IERR)
            END IF
         IF (IERR.EQ.0) THEN
            MSGTXT = '''' // PHNAME(:J) // ''' deleted'
            CALL MSGWRT (3)
            RDUM = -1.0
         ELSE
            WRITE (MSGTXT,1905) IERR, PHNAME(:J)
            CALL MSGWRT (7)
            RDUM = 1.0
            END IF
         END IF
      CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      GO TO 999
C-----------------------------------------------------------------------
C                                       REVERSN
C                                       find all versions of ext file
C-----------------------------------------------------------------------
 910  CALL ADVERB ('INEXT', 'C', 1,  2, IDUM, RDUM, XXTYPE)
      IF (ERRNUM.NE.0) GO TO 990
      SCAT = 1
      POTERR = 45
      IMTYPE = ' '
      IUSER = NLUSER
      CALL CATDIR ('SRCH', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ, IMTYPE,
     *    IUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       one found: read catlg block
      CALL CATIO ('READ', IVOL, SCAT, CATBLK, 'WRIT', BUFF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         MSGTXT = 'UNABLE TO SET WRITE STATUS, FILE BUSY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       single specified type
      IF (XXTYPE.NE.' ') THEN
         DO 920 I = 1,KIEXTN
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), HDTYPE)
            IF (HDTYPE.EQ.XXTYPE) THEN
               VER = 0
               MISS = 0
               DO 915 J = 1,46655
                  CALL ZPHFIL (HDTYPE, IVOL, SCAT, J, PHNAME, IERR)
                  CALL ZEXIST (IVOL, PHNAME, ISIZE, IERR)
                  IF (IERR.EQ.0) THEN
                     VER = J
                     MISS = 0
                  ELSE IF (IERR.EQ.1) THEN
                     MISS = MISS + 1
                     IF (MISS.GT.200) GO TO 916
                  ELSE IF (IERR.GT.1) THEN
                     WRITE (MSGTXT,1910) IERR, HDTYPE, J
                     GO TO 980
                     END IF
 915              CONTINUE
 916           IF (VER.NE.CATBLK(KIVER+I-1)) THEN
                  WRITE (MSGTXT,1915) HDTYPE, CATBLK(KIVER+I-1), VER
                  CALL MSGWRT (4)
                  CATBLK(KIVER+I-1) = VER
                  END IF
               GO TO 970
               END IF
 920        CONTINUE
C                                       do all possible
      ELSE
         DO 950 IC = 1,26
            DO 945 JC = 1,26
               XXTYPE = ALPH(IC:IC) // ALPH(JC:JC)
               DO 925 I = 1,NONUMB
                  IF (XXTYPE.EQ.NONAME(I)) GO TO 945
 925              CONTINUE
               HPOS = KIEXTN + 1
               DO 930 I = 1,KIEXTN
                  CALL H2CHR (2, 1, CATH(KHEXT+I-1), HDTYPE)
                  IF (HDTYPE.EQ.' ') THEN
                     HPOS = MIN (I, HPOS)
                  ELSE IF (HDTYPE.EQ.XXTYPE) THEN
                     GO TO 935
                     END IF
 930              CONTINUE
               I = HPOS
 935           VER = 0
               MISS = 0
               DO 940 J = 1,46655
                  CALL ZPHFIL (XXTYPE, IVOL, SCAT, J, PHNAME, IERR)
                  CALL ZEXIST (IVOL, PHNAME, ISIZE, IERR)
                  IF (IERR.EQ.0) THEN
                     VER = J
                     MISS = 0
                  ELSE IF (IERR.EQ.1) THEN
                     MISS = MISS + 1
                     IF ((MISS.EQ.J) .AND. (MISS.GT.15)) GO TO 941
                     IF (MISS.GT.100) GO TO 941
                  ELSE IF (IERR.GT.1) THEN
                     WRITE (MSGTXT,1910) IERR, XXTYPE, J
                     GO TO 980
                     END IF
 940              CONTINUE
 941           IF (VER.GT.0) THEN
                  IF (I.GT.KIEXTN) THEN
                     MSGTXT = 'ALL HEADER SLOTS USED UP ' // XXTYPE //
     *                  ' LEFT OUT'
                     CALL MSGWRT (8)
                  ELSE IF (VER.NE.CATBLK(KIVER+I-1)) THEN
                     WRITE (MSGTXT,1915) XXTYPE, CATBLK(KIVER+I-1), VER
                     CALL MSGWRT (4)
                     CATBLK(KIVER+I-1) = VER
                     CALL CHR2H (2, XXTYPE, 1, CATH(KHEXT+I-1))
                     END IF
                  END IF
 945           CONTINUE
 950        CONTINUE
         END IF
C                                       update header
 970  CALL CATIO ('UPDT', IVOL, SCAT, CATBLK, 'CLWR', BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1970) IERR
      ELSE
         POTERR = 0
         GO TO 990
         END IF
C                                       error fix status if possible
 980  CALL MSGWRT (8)
      IMTYPE = ' '
      CALL CATDIR ('CSTA', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ, IMTYPE,
     *   IUSER, 'CLWR', BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
C-----------------------------------------------------------------------
C                                        errors
 990  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1300 FORMAT ('Keyword = ''',A8,'''  value = ',1PD15.8)
 1301 FORMAT ('Keyword = ''',A8,'''  value = ',1PE13.6)
 1302 FORMAT ('Keyword = ''',A8,'''  value = ''',A8,'''')
 1303 FORMAT ('Keyword = ''',A8,'''  value = ',I12)
 1304 FORMAT ('Keyword = ''',A8,'''  value = ',L1)
 1305 FORMAT ('KEYWORD = ''',A8,'''  INVALID KEYTYPE = ',I12)
 1400 FORMAT ('I WON''T DESTROY ON DEFAULT DISK OR TYPE',
     *   ' INDISK, INTYPE=',I3,1X,A2)
 1405 FORMAT ('I WON''T DESTROY ON DEFAULT NAME INNAME = ''',A12,'''')
 1800 FORMAT ('ERROR',I5,' LOOKING FOR FILE EXISTANCE')
 1810 FORMAT ('Main file size',F10.3,' Megabytes')
 1815 FORMAT ('IMAGR workfile size',F10.3,' Megabytes or less')
 1900 FORMAT ('Destroy file ''',A,'''   Enter YES or NO')
 1905 FORMAT ('ERROR',I3,' ''',A,''' NOT DESTOYED')
 1910 FORMAT ('ZEXIST ERROR',I2,' ON EXTENSION ',A,' VERS',I6)
 1915 FORMAT ('REVERSN changed type ',A2,' from',I6,' to',I6,
     *   ' versions')
 1970 FORMAT ('CATIO ERROR',I4,' ON ATTEMPT TO UPDATE HEADER')
 1980 FORMAT ('CATDIR ERROR',I4,' ON ATTEMPT TO CLEAR WRITE STATUS')
      END
