      SUBROUTINE STORES (BRANCH)
C-----------------------------------------------------------------------
C! stores proc code; pseudoverbs: SAVE, GET, RESTORE, STORE, LIST, ...
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2003, 2013
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
C   STORES stores either the procedure source code, procedure object
C   code, or handles the source code.  Note BRANCH 1 is an internal
C   operator that stores the source and object code for procedures.
C   Inputs: BRANCH  operation code number
C                   1 : Store Proc source & object code
C                   2 : STORE <n> save K array + LISTF
C                   3 : SAVE <ln> save K & LISTF in user area
C                   4 : LIST <proc> source code  on terminal
C                   5 : CORE used/avail report
C                   6 : RESTORE <n> recover STOREd K & LISTF
C                   7 : GET <ln> recover user area to K & LISTF
C                   8 : SCRATCH <proc> delete proc object code
C                   9 : COMPRESS is stubbed now
C                  10 : MODIFY a line of a procedure
C   LISTF is the storage location for PROC source code and is now
C   kept in common.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PHNAME*48, IOPRAT*4, TEMP*80, PRGNAM*6, SPACE*1,
     *   DELETE*1, INSERT*1, ICHAR*1, SCNAME*24
      INTEGER   BPR(100), A(100), B(100), ITEMP, POTERR, FINDME, LBYTES,
     *   LUN, FIND, LUNSG, IT, IVER, MSGIND, LISTL, LL, IERR, I, J, IER,
     *   II, IOP, IX, JJ, KBP, KP, KX3, KX5, MSGLUN,  LUNME, NMEM, INP,
     *   KOP, LLOCAT, IL, LLL, NWORDS, MSGLIN, ISCR(256), ILINK, ISIZE,
     *   IDPI, LUNDE, LBRNCH, SAVEU, LUNTMP
      LOGICAL   T, F, WASFIL, DOINS, FIXIT, EXCL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DRUN.INC'
      EQUIVALENCE (BPR(1), V(1))
      EQUIVALENCE (STACK(1), A(1)),   (CSTACK(1), B(1))
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNME, LUNDE /14,10/
      DATA MSGLUN, LUNSG /6, 27/
      DATA PRGNAM /'STORES'/
      DATA SPACE, DELETE, INSERT /'@','$','_'/
C-----------------------------------------------------------------------
      FIXIT = .FALSE.
      LBRNCH = BRANCH
      POTERR = 76
      IF ((LBRNCH.LT.1) .OR. (LBRNCH.GT.11)) GO TO 980
      POTERR = 71
      IF ((MODE.EQ.1) .AND. (LBRNCH.GE.2) .AND. (LBRNCH.LE.9))
     *   GO TO 980
      IF ((MODE.EQ.1) .AND. (LBRNCH.GE.11)) GO TO 980
      WASFIL = .TRUE.
C                                       branch to operation
      GO TO (100, 200, 210, 220, 240, 300, 320, 400, 450, 500, 600),
     *   LBRNCH
C-----------------------------------------------------------------------
C                                  Store PROC source and object code.
C-----------------------------------------------------------------------
C                                       Store object code.
 100  ITEMP = AP + 2
      L = LLOCAT (ITEMP, K, LPGM)
      IF (ERRNUM.NE.0) GO TO 970
      LPGM = L
      L = L + 1
      LISTL = 1
      II = (NBYTES+3) / 4 + 1
      K(L) = LLOCAT (II, LISTF, LISTL)
      IF (ERRNUM.NE.0) GO TO 970
      L = L + 1
      CALL COPY (AP, A, K(L))
C                                       Store source code.
      LISTF(LISTL) = NBYTES
      LISTL = LISTL + 1
      KBP = 1
      KP = 1
      CALL CHR2H (NBYTES, KARBUF, 1,  LISTF(LISTL))
      LISTL = LISTL + II - 1
      GO TO 950
C-----------------------------------------------------------------------
C                                       STORE - all memory.
C-----------------------------------------------------------------------
 200  IOP = 2
      IOPRAT = 'WRIT'
      STORE1 = 1
      STORE2 = 1
      GO TO 310
C-----------------------------------------------------------------------
C                                       SAVE
C                                       to user store area
C-----------------------------------------------------------------------
 210  IOP = 2
      IOPRAT = 'WRIT'
      GO TO 330
C-----------------------------------------------------------------------
C                                       LIST a Procedure.
C-----------------------------------------------------------------------
 220  CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 970
C                                       Error check.
      POTERR = 11
      IF (SYTYPE.NE.3) GO TO 970
      IX = 1
      ILINK = TAG
 230  L = ILINK
         ILINK = K(L)
         IT = K(L+1)
         IF (IT.EQ.0) GO TO 230
            LL = IT + 1
            LLL = (LISTF(IT)+3) / 4
            WRITE (MSGTXT,1230) IX
            CALL H2CHR (LISTF(IT), 1, LISTF(LL), MSGTXT(4:))
            CALL MSGWRT (3)
            IX = IX + 1
            IF (ILINK.NE.0) GO TO 230
      GO TO 950
C-----------------------------------------------------------------------
C                                       Report CORE utilization.
C-----------------------------------------------------------------------
 240  KX5 = K(8) + 10
      KX3 = K(KXORG+3-1) - KX5
      KX5 = K(KXORG+5-1) - KX5
      WRITE (MSGTXT,1240)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1241)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1242) K(3), K(5), KX3, KX5, LISTF(3), LISTF(5)
      CALL MSGWRT (4)
      GO TO 950
C-----------------------------------------------------------------------
C                                       RESTORE - all memory.
C-----------------------------------------------------------------------
 300  IOP = 1
      IOPRAT = 'READ'
C                                       STORE and RESTORE
C                                       get region number
 310  CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 970
C                                       Error check.
      POTERR = 12
      IF (SYTYPE.NE.11) GO TO 970
      POTERR = 32
      NMEM = X(1)
      IF ((NMEM.LT.IOP-1) .OR. (NMEM.GT.1)) GO TO 970
      IF ((NMEM.EQ.1) .AND. (IOP.EQ.1)) THEN
         IF (STORE1.NE.1) THEN
            POTERR = 101
            MSGTXT = 'NO STORE 1 HAS BEEN DONE DURING THIS JOB'
            CALL MSGWRT (8)
            GO TO 980
            END IF
         IF (STORE2.NE.1) THEN
            STORE2 = 1
            MSGTXT= 'Warning: RESTORE 1 area was STOREd by another user'
            CALL MSGWRT (6)
            END IF
         END IF
      IDPI = 1
C                                       Open ME file
 315  I = NPOPS * NMEM
      CALL ZPHFIL ('ME', 1, 0, I, PHNAME, IERR)
      EXCL = T
      IF (I.EQ.0) EXCL = F
      CALL ZOPEN (LUNME, FINDME, 1, PHNAME, F, EXCL, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 980
      LUN = LUNME
      FIND = FINDME
      IDPI = 1
      GO TO 340
C-----------------------------------------------------------------------
C                                       GET
C                                       from user store area
C-----------------------------------------------------------------------
 320  IOP = 1
      IOPRAT = 'READ'
C                                       SAVE and GET
C                                       parse area name: char,#
 330  CALL SGLOCA (IOP, IVER, FIXIT)
      IF ((FIXIT) .AND. (IOPRAT.EQ.'READ') .AND. (ERRNUM.EQ.30))
     *   ERRNUM = 0
      IF (ERRNUM.NE.0) GO TO 970
      CALL ZPHFIL ('SG', 1, NLUSER, IVER, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       create file if needed
      IF (IOPRAT.NE.'READ') THEN
         ISIZE = MPAGE + 1
         CALL ZEXIST (1, PHNAME, IDPI, IERR)
         IF (IERR.EQ.0) CALL ZDESTR (1, PHNAME, IERR)
         WASFIL = IERR.NE.1
C                                       create it now
         IF ((IERR.EQ.0) .OR. (IERR.EQ.1)) CALL ZCREAT (1, PHNAME,
     *      ISIZE, F, IDPI, IERR)
         IF ((IERR.LT.0) .OR. (IERR.GT.1)) THEN
            POTERR = 50
            GO TO 970
            END IF
C                                       message overwrite
         IF (WASFIL) THEN
            MSGTXT = 'OVERWRITING EXISTING FILE'
            CALL MSGWRT (6)
            WASFIL = .FALSE.
            END IF
         END IF
C                                       open SG file
      CALL ZOPEN (LUNSG, FIND, 1, PHNAME, F, T, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 965
      IDPI = 2
      LUN = LUNSG
C                                       STORE, RESTORE, SAVE, GET
C                                       move K array
 340  JJ = MPAGE - LPAGE
      POTERR = 61
      J = 1
      DO 345 I = 1,JJ
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, K(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 345     CONTINUE
C                                       move LISTF
      J = 1
      DO 350 I = 1,LPAGE
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, LISTF(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 350     CONTINUE
C                                       SAVE SG files: write time
      IF (LBRNCH.EQ.3) THEN
         CALL FILL (256, 0, ISCR)
         CALL ZDATE (ISCR(1))
         CALL ZTIME (ISCR(4))
         IDPI = 1
         CALL ZFIO ('WRIT', LUNSG, FIND, IDPI, ISCR, IERR)
         END IF
      IF ((FIXIT) .AND. (LBRNCH.EQ.7)) THEN
         FIXIT = .FALSE.
         LBRNCH = 9
         CALL ZCLOSE (LUNSG, FIND, IERR)
         MSGTXT = 'GET file being modernized via COMPRESS.' //
     *      '  You should SAVE it.'
         CALL MSGWRT (6)
C                                       reset file size
         LPAGE = LBLOCK
         MPAGE = KBLOCK + LPAGE
         GO TO 450
         END IF
      GO TO 950
C-----------------------------------------------------------------------
C                                       Scratch a Procedure.
C-----------------------------------------------------------------------
 400  CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 11
      IF (SYTYPE.NE.3) GO TO 980
C                                        protect VLAGEN'd procs
      POTERR = 25
      IF (L.LT.K(7)) GO TO 980
C                                       Search for the Procedure.
      L = 1
 410  LAST = L
         L = K(L)
         IF (TAG.NE.K(L+2)) GO TO 410
C                                       Delete PROC from symbol table.
      IF (IDEBUG.GT.0) THEN
         WRITE (MSGTXT,1410) LAST, L, K(L)
         CALL MSGWRT (4)
         END IF
C                                          Last symbol entry.
      IF (K(L).EQ.0) THEN
         K(9) = LAST
         K(LAST) = 0
C                                          Middle symbol entry.
      ELSE
         K(LAST) = K(L)
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Compress PROCEDURES.
C-----------------------------------------------------------------------
C                                       save the procs and adverb values
 450  SCNAME = 'RUNFIL:COMPRESS.N.UUU'
      SCNAME(12:15) = HSTNAM(:4)
      CALL ZEHEX (NPOPS, 1, SCNAME(17:17))
      CALL ZEHEX (NLUSER, 3, SCNAME(19:21))
C                                       get an LUN
      IF (NUMRUN.GE.MAXRUN) THEN
         MSGTXT = 'MORE THAN 20 DEEP IN RUN FILES'
         CALL MSGWRT (8)
         ERRNUM = 101
         GO TO 980
         END IF
      NUMRUN = NUMRUN + 1
      IF (NUMRUN.EQ.1) THEN
         LUN = LUNDE
      ELSE IF ((LUNRUN(NUMRUN).GT.0) .AND. (LUNRUN(NUMRUN).LE.99)) THEN
         LUN = LUNRUN(NUMRUN)
      ELSE
         LUN = LUNTMP (2)
         END IF
      LUNRUN(NUMRUN) = LUN
      CALL K2TEXT (LUN, .TRUE., SCNAME)
      IF (ERRNUM.NE.0) GO TO 980
C                                       set to type 5 input
      IUNIT = 5
C                                       do a RESTORE 0
      IOP = 1
      IOPRAT = 'READ'
      NMEM = 0
      GO TO 315
C-----------------------------------------------------------------------
C                                       MODIFY a line of a proc
C-----------------------------------------------------------------------
C                                       Find source code line
 500  LINK = NAMEP
      POTERR = 53
      IF (IUNIT.EQ.3) POTERR = 60
      IF (IUNIT.NE.1) GO TO 970
      IX = 0
      IL = XX
      POTERR = 11
      IF (IL.LT.2) GO TO 970
 510  L = LINK
         IF (L.EQ.0) GO TO 970
         LINK = K(L)
         IT = K(L+1)
         IF (IT.EQ.0) GO TO 510
            IX = IX + 1
            IF (IX.LT.IL) GO TO 510
               LL = IT + 1
               LBYTES = LISTF(IT)
               NWORDS = (LBYTES+3) / 4
C                                       Echo old line
      TEMP = '?'
      CALL H2CHR (LBYTES, 1, LISTF(LL), TEMP(2:))
      I = LBYTES + 1
      CALL ZMSGOP (MSGLUN, MSGIND, 1, PHNAME, IERR)
      CALL ZTTYIO ('WRIT', MSGLUN, MSGIND, I, TEMP, IERR)
      CALL ZMSGCL (MSGLUN, MSGIND, IERR)
C                                       Read new line
      IPT = '?'
      CALL PREAD (KARBUF)
      IPT = '>'
      IF (ERRNUM.NE.0) GO TO 970
      IF (NBYTES.LE.0) GO TO 550
C                                       Merge old and new lines
      INP = 1
      KOP = 1
      DOINS = .FALSE.
      DO 520 I = 1,1000
         ICHAR = KARBUF(I:I)
C                                       Insert mode
         IF (DOINS) THEN
            IF ((ICHAR.EQ.INSERT) .OR. (I.GT.NBYTES)) THEN
               DOINS = .FALSE.
            ELSE
               KARBUF(KOP:KOP) = ICHAR
               IF (ICHAR.EQ.SPACE) KARBUF(KOP:KOP) = ' '
               IF (ICHAR.NE.DELETE) KOP = KOP + 1
               END IF
C                                       Substitute mode
         ELSE IF ((I.LE.NBYTES) .OR. (INP.LE.LBYTES)) THEN
C                                       Pure copy at end
            IF (I.GT.NBYTES) THEN
               CALL H2CHR (1, INP, LISTF(LL), KARBUF(KOP:KOP))
               KOP = KOP + 1
               INP = INP + 1
C                                       Start insert
            ELSE IF (ICHAR.EQ.INSERT) THEN
               DOINS = .TRUE.
C                                       Delete a character
            ELSE IF (ICHAR.EQ.DELETE) THEN
               INP = INP + 1
C                                       Change to blank
            ELSE IF (ICHAR.EQ.SPACE) THEN
               KARBUF(KOP:KOP) = ' '
               KOP = KOP + 1
               INP = INP + 1
C                                       Keep old character
            ELSE IF (ICHAR.EQ.' ') THEN
               KARBUF(KOP:KOP) = ' '
               IF (INP.LE.LBYTES) CALL H2CHR (1, INP, LISTF(LL),
     *            KARBUF(KOP:KOP))
               INP = INP + 1
               KOP = KOP + 1
C                                       New character
            ELSE
               KARBUF(KOP:KOP) = ICHAR
               IF ((ICHAR.EQ.SPACE) .OR. (ICHAR.EQ.INSERT) .OR.
     *            (ICHAR.EQ.DELETE)) KARBUF(KOP:KOP) = ' '
               KOP = KOP + 1
               INP = INP + 1
               END IF
         ELSE
            GO TO 530
            END IF
 520     CONTINUE
C                                       Count real characters
 530  NBYTES = NCHLIN
      INP = NCHLIN - KOP + 1
      IF (INP.GT.0) KARBUF(KOP:NCHLIN) = ' '
      DO 535 I = 1,NCHLIN
         ICHAR = KARBUF(NBYTES:NBYTES)
         IF ((ICHAR.NE.' ') .AND. (ICHAR.NE.SPACE) .AND.
     *      (ICHAR.NE.DELETE)) GO TO 540
            KARBUF(NBYTES:NBYTES) = ' '
            NBYTES = NBYTES - 1
 535     CONTINUE
C                                       Set GETFLD pointer to start line
 540  KBPTR = 1
C                                       Echo new line
      TEMP = ' ' // KARBUF(1:NBYTES)
      I = NBYTES + 1
      CALL ZMSGOP (MSGLUN, MSGIND, 1, PHNAME, IERR)
      CALL ZTTYIO ('WRIT', MSGLUN, MSGIND, I, TEMP, IERR)
      CALL ZMSGCL (MSGLUN, MSGLIN, IERR)
C                                       New line blank or changed?
      IF (NBYTES.LE.LBYTES) GO TO 950
      CALL H2CHR (NBYTES, 1, LISTF(LL), MSGTXT)
      IF (MSGTXT.NE.TEMP(2:)) GO TO 950
C                                       No: user changed mind
 550  MODE = 0
      AP = 0
      GO TO 950
C-----------------------------------------------------------------------
C                                       USAVE - save to another user
C-----------------------------------------------------------------------
C                                       get user number
 600  CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 970
C                                       Error check.
      POTERR = 12
      IF (SYTYPE.NE.11) GO TO 970
      POTERR = 32
      NMEM = X(1)
      IF ((NMEM.LT.1) .OR. (NMEM.GT.46655)) GO TO 970
      SAVEU = NLUSER
      IOP = 2
      IOPRAT = 'WRIT'
      NLUSER = NMEM
      CALL SGLOCA (IOP, IVER, FIXIT)
      NLUSER = SAVEU
      IF (ERRNUM.NE.0) GO TO 970
      NLUSER = NMEM
      CALL ZPHFIL ('SG', 1, NLUSER, IVER, PHNAME, IERR)
      NLUSER = SAVEU
      IF (IERR.NE.0) GO TO 970
C                                       create file
      ISIZE = MPAGE + 1
      CALL ZEXIST (1, PHNAME, IDPI, IERR)
      IF (IERR.EQ.0) CALL ZDESTR (1, PHNAME, IERR)
      WASFIL = IERR.NE.1
C                                       create it now
      IF ((IERR.EQ.0) .OR. (IERR.EQ.1)) CALL ZCREAT (1, PHNAME,
     *   ISIZE, F, IDPI, IERR)
      IF ((IERR.LT.0) .OR. (IERR.GT.1)) THEN
         POTERR = 50
         GO TO 970
         END IF
C                                       message overwrite
      IF (WASFIL) THEN
         MSGTXT = 'OVERWRITING EXISTING FILE'
         CALL MSGWRT (6)
         WASFIL = .FALSE.
         END IF
C                                       open SG file
      CALL ZOPEN (LUNSG, FIND, 1, PHNAME, F, T, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 965
      IDPI = 2
      LUN = LUNSG
C                                       move K array
      JJ = MPAGE - LPAGE
      POTERR = 61
      J = 1
      DO 610 I = 1,JJ
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, K(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 610     CONTINUE
C                                       move LISTF
      J = 1
      DO 620 I = 1,LPAGE
         CALL ZFIO (IOPRAT, LUN, FIND, IDPI, LISTF(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 960
         IDPI = IDPI + 1
         J = J + 256
 620     CONTINUE
C                                       SAVE SG files: write time
      CALL FILL (256, 0, ISCR)
      CALL ZDATE (ISCR(1))
      CALL ZTIME (ISCR(4))
      IDPI = 1
      CALL ZFIO ('WRIT', LUNSG, FIND, IDPI, ISCR, IERR)
      GO TO 950
C-----------------------------------------------------------------------
C                                       Close down: normal
 950  POTERR = 0
      WASFIL = .TRUE.
C                                       close SG
 960  IF ((LBRNCH.NE.3) .AND. (LBRNCH.NE.7) .AND. (LBRNCH.NE.11))
     *   GO TO 970
         CALL ZCLOSE (LUNSG, FIND, IERR)
C                                       destroy new SG on err
 965     IF (.NOT.WASFIL) CALL ZDESTR (1, PHNAME, IERR)
C                                       close file ME
 970  IF ((LBRNCH.EQ.2) .OR. (LBRNCH.EQ.6) .OR. (LBRNCH.EQ.9))
     *   CALL ZCLOSE (LUNME, FINDME, IER)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1230 FORMAT (I2)
 1240 FORMAT (16X,'Space used / total for')
 1241 FORMAT (7X,'Program',9X,'Variables',9X,'Source')
 1242 FORMAT (3X,3(I6,' /',I6,3X))
 1410 FORMAT ('PROC found: LAST,L,K(L)',3I6)
      END
