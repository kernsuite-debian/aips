      SUBROUTINE HELPS (BRANCH)
C-----------------------------------------------------------------------
C! executes "pseudoverb name" -> hidden verb w name on stack (INP, RUN)
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2002, 2004, 2007, 2013
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
C   HELPS contains the user assistance facilities: HELP, RUN, and
C   INPUTS.  HELP lists symbols by type or lists a text file whose
C   member name matches a user  name.  RUN sets the input to a
C   specified member of a text file (allowing users to to have personal
C   strings of commands (e.g. procs, verbs, adverb settings)) which is
C   executed until an EOF.  INPUTS lists the adverbs and their current
C   values and brief descriptions on the terminal.  A text file is used
C   there also.
C   Inputs:
C      BRANCH   I   1 -> RUN (set input device to file)
C                   2 -> HELP (list text file or symbols)
C                   3 -> EXPLAIN (print Help/Explain file)
C                   4 -> APROPOS (all helps with this word)
C                   5 -> INP (fast INPUTS)
C                   6 -> INPUTS (list adverbs & values)
C                   7 -> SHOW
C                   8 -> GO (start a task)
C                   9 -> TELL
C                  10 -> TGET (restore Task's adverbs)
C                  11 -> TPUT (save adverbs in TGET file)
C                  12 -> SGDESTR (destroy 1 Save/Get file)
C                  13 -> ABORTASK (abort a task)
C                  14 -> WAITTASK (wait for task to end)
C                  15 -> ABOUT
C                  16 -> XHELP (hypertext help)
C                  17 -> VGET  (task adverb versioned get)
C                  18 -> VPUT  (task adverb versioned put)
C                  19 -> DEFAULT (reset adverbs for 1 task only)
C                  20 -> OUTPUTS (display output adverbs)
C
C   This subroutine only performs setup functions.  The IO routines
C   carry out the RUN and the routine AU1A does HELP, INP, INPUTS, and
C   SHOW while GO, ABORTASK, TELL, and TPUT are done by AU2 and TGET
C   and SGDESTR are done by AU2A.
C   For these, HELPS sets up the requested <name>
C   as a hollerith string on the stack with a "verb" # following.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, MEMBER*64, PHNAME*48, LVERSN*48, AVERSN*48,
     *   JBUF*80
      INTEGER   IMAX, POTERR, LUN, RUNLUN, FIND, IEOF, IERR, A(100),
     *   IVERB, NCLIM, VERBNO(19), IDUM, LUNTMP
      REAL      RDUM
      LOGICAL   WAIT
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DRUN.INC'
      EQUIVALENCE (A(1), STACK(1))
      DATA RUNLUN /10/
      DATA WAIT /.TRUE./
      DATA PRGNAM /'HELPS '/
      DATA VERBNO /210, 213, 215, 211, 212, 214, 220, 225, 230, 224,
     *   231, 223, 222, 216, 217, 235, 229, 236, 218/
C-----------------------------------------------------------------------
      IEOF = 0
      POTERR = 76
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.20)) GO TO 980
      IF (BRANCH.NE.1) GO TO 200
C-----------------------------------------------------------------------
C                                       RUN <NAME>.
C-----------------------------------------------------------------------
C                                        Batch AIPSs receive any
C                                        RUN files already
C                                        expanded into input stream
 100  POTERR = 71
      IF (MODE.EQ.1) GO TO 980
      IMAX = NINTRN
      IF (NBATQS.GT.0) IMAX = IMAX + 1
      IF (NPOPS.GT.IMAX) GO TO 999
      SP = 0
C                                        <name> in KARBUF -> MEMBER
      CALL GETNME (' ', 8, MEMBER)
      IF (ERRNUM.NE.0) GO TO 980
C                                        no name -> exit
      IF (MEMBER.EQ.' ') GO TO 999
C                                        Create name. Open file.
      POTERR = 101
      CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
      IF (ERRNUM.NE.0) GO TO 980
      IF (LVERSN.EQ.' ') LVERSN = VERNAM(1:3)
      CALL ZPHFIL ('RU', 1, 0, 0, PHNAME, IERR)
      CALL VERMAT (1, PHNAME, MEMBER, LVERSN, IVERB, AVERSN, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       get an LUN
      IF (NUMRUN.GE.MAXRUN) THEN
         MSGTXT = 'MORE THAN 20 DEEP IN RUN FILES'
         CALL MSGWRT (8)
         ERRNUM = 101
         GO TO 980
         END IF
      NUMRUN = NUMRUN + 1
      IF (NUMRUN.EQ.1) THEN
         LUN = RUNLUN
      ELSE IF ((LUNRUN(NUMRUN).GT.0) .AND. (LUNRUN(NUMRUN).LE.99)) THEN
         LUN = LUNRUN(NUMRUN)
      ELSE
         LUN = LUNTMP (2)
         END IF
      LUNRUN(NUMRUN) = LUN
      CALL ZTOPEN (LUN, FIND, 1, PHNAME, MEMBER, AVERSN, WAIT, IERR)
      IF (IERR.NE.0) THEN
         NUMRUN = NUMRUN - 1
         GO TO 980
         END IF
C                                        skip first record (comment)
      CALL ZTREAD (LUN, FIND, JBUF, IERR)
      IF (IERR.NE.0) THEN
         NUMRUN = NUMRUN - 1
         GO TO 970
         END IF
C                                        set unit to text disk. GTLINE
C                                        will read rest of file.
      IUNIT = 2
      GO TO 999
C-----------------------------------------------------------------------
C                                       HELP     <name>
C                                       EXPLAIN  <name>
C                                       APROPOS  <name>
C                                       INP      <name>
C                                       INPUTS   <name>
C                                       SHOW     <name>
C                                       GO       <name>
C                                       TELL     <name>
C                                       TGET     <name>
C                                       TPUT     <name>
C                                       SGDESTR  <name>
C                                       ABORTASK <name>
C                                       WAITTASK <name>
C                                       ABOUT    <name>
C                                       XHELP    <name>
C                                       VGET     <name>
C                                       VPUT     <name>
C                                       DEFAULT  <name>
C-----------------------------------------------------------------------
 200  NCLIM = 8
      IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3) .OR. (BRANCH.EQ.15) .OR.
     *   (BRANCH.EQ.16))NCLIM = 10
      IF (BRANCH.EQ.4) NCLIM = 64
      IF (BRANCH.EQ.12) NCLIM = 16
      CALL GETNME (' ', NCLIM, MEMBER)
      IF (ERRNUM.NE.0) GO TO 980
C                                       store as litoral
      CALL RFILL(5, HBLANK, X)
      CALL CHR2H (NCLIM, MEMBER, 1, X)
      SYTYPE = 14
      LX = (NCLIM + 3) / 4
      CALL LTSTOR
      IF (ERRNUM.EQ.1) ERRNUM = 75
      IF (ERRNUM.NE.0) GO TO 980
      K(LOCSYM+1) = SYTYPE + 16*NCLIM
C                                       litoral on stack
      CALL PUSH (A, AP, -SYTYPE)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (A, AP, -NCLIM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (A, AP, -TAG)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (A, AP, -2)
      IF (ERRNUM.NE.0) GO TO 980
C                                       verb # for this pseudoverb
      IVERB = VERBNO(BRANCH-1)
C                                       GO: is it a verb (not task)
      IF ((BRANCH.EQ.8) .AND. (NKAR.GT.0)) THEN
         CALL RCOPY (5, X, KPAK)
         CALL SYMBOL (1)
C                                       Yes: do TPUT, then verb
         IF ((ERRNUM.EQ.0) .AND. ((SYTYPE.EQ.4) .OR. (SYTYPE.EQ.3)))
     *      THEN
            IVERB = VERBNO(10)
            ERRNUM = 0
            CALL PUSH (A, AP, IVERB)
            IF (ERRNUM.NE.0) GO TO 980
            IVERB = TAG
         ELSE
            ERRNUM = 0
            END IF
         END IF
      CALL PUSH (A, AP, IVERB)
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                        error return
C-----------------------------------------------------------------------
 970  CALL ZTCLOS (LUN, FIND, IERR)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
