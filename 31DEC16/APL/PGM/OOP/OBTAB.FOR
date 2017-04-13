LOCAL INCLUDE 'POINT.INC'
C
C     Information about source pointings
C
C     MAXPNT   Maximum number of source pointings
C     NPOINT   Actual number of source pointings
C     STIME    Start time for each pointing (days)
C     ETIME    End time for each pointing (days)
C     RAS      Right ascensions of pointings (radians)
C     DECS     Declinations of pointings (radians)
C
      INTEGER   MAXPNT, NPOINT
      PARAMETER (MAXPNT = 1000)
C
      DOUBLE PRECISION STIME(MAXPNT), ETIME(MAXPNT), RAS(MAXPNT),
     *                 DECS(MAXPNT)
C
      COMMON /POINT/ STIME, ETIME, RAS, DECS, NPOINT
      SAVE /POINT/
C
LOCAL END
      PROGRAM OBTAB
C-----------------------------------------------------------------------
C! Update information for orbiting antennae.
C# Task Table
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2009, 2015
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
C
C     Local variables
C
C     INPUTS      Name of INPUTS object
C     ANTAB       Name of antenna TABLE object
C     ORBTAB      Name of orbit TABLE object
C
C     OANTS       IDs of antennae to be updated
C     OCOUNT      Number of antennae to be updated
C     SUBARR      Subarray to be updated
C
C     SEMIMA      Semimajor axis of orbit/m
C     ECCEN       Eccentricity of orbit
C     INCLIN      Inclination of orbit/degrees
C     RAANOD      Right ascension of ascending node/degrees
C     APERIG      Argument of perigee/degrees
C     MANMLY      Mean anomaly at 0.0 days/degrees
C
C     IRET        Task completion status (0 - ends successfully;
C                                         other - fails)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER INPUTS*6, ANTAB*5, ORBTAB*5
      INTEGER   OANTS(MAXANT), OCOUNT, SUBARR
      DOUBLE PRECISION SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY
      INTEGER   IRET
      INTEGER   BUFFER(256)
C
      PARAMETER (INPUTS = 'INPUTS')
      PARAMETER (ANTAB  = 'ANTAB')
      PARAMETER (ORBTAB = 'ORBTAB')
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PEARTH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Initialize task:
C
      CALL OBTINI (INPUTS, ANTAB, ORBTAB, OANTS, OCOUNT, SUBARR, SEMIMA,
     *             ECCEN, INCLIN, RAANOD, APERIG, MANMLY, IRET)
      IF (IRET.EQ.0) THEN
C
C        The following conditions should hold:
C        INPUTS is initialized
C        SUBARR > 0
C        ANTAB is initialized
C        ANTAB.VERS = SUBARR
C        ORBTAB is initialized if SEMIMA < ERAD
C        0 < OCOUNT <= MAXANT
C        for all i, 1 <= i <= OCOUNT, OANTS(i) > 0
C
         IF (SEMIMA.LT.ERAD) THEN
C
C           Scan and update OB table:
C
            CALL UPDOTB (ORBTAB, OANTS, OCOUNT, SUBARR, IRET)
            IF (IRET.EQ.0) THEN
C
C              Extract orbital elements calculated from OB table:
C
               CALL OELEM (ORBTAB, SEMIMA, ECCEN, INCLIN, RAANOD,
     *                     APERIG, MANMLY, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'COULD NOT DERIVE ORBITAL ELEMENTS'
                  CALL MSGWRT (9)
               END IF
            ELSE
               MSGTXT = 'FAILED TO UPDATE ORBIT TABLE'
               CALL MSGWRT (9)
            END IF
         END IF
C
C        IRET = 0 implies that the orbital elements have their
C        correct values.
C
         IF (IRET.EQ.0) THEN
            CALL UPDATB (ANTAB, OANTS, OCOUNT, SEMIMA, ECCEN, INCLIN,
     *                   RAANOD, APERIG, MANMLY, IRET)
            IF (IRET.EQ.0) THEN
               CALL OBHIST (INPUTS, ANTAB, ORBTAB, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'FAILED TO UPDATE HISTORY FILE'
                  CALL MSGWRT (9)
               END IF
            ELSE
               MSGTXT = 'FAILED TO UPDATE ANTENNA TABLE'
               CALL MSGWRT (9)
            END IF
         END IF
      END IF
C
      CALL DIE (IRET, BUFFER)
      END
      SUBROUTINE OBTINI (INPUTS, ANTAB, ORBTAB, OANTS, OCOUNT, SUBARR,
     *                   SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY,
     *                   IRET)
C-----------------------------------------------------------------------
C   Read the task adverbs and detach from POPS.
C
C   Inputs:
C      INPUTS     C*(*)          Name of INPUTS object for task adverbs
C      ANTAB      C*(*)          Name of TABLE object for antenna table
C      ORBTAB      C*(*)          Name of TABLE object for orbit table
C
C   Outputs:
C      OANTS      I(*)           List of antennae to modify
C      OCOUNT     I              Number of antennae to modify
C      SUBARR     I              Subarray to modify
C      SEMIMA     D              Semimajor axis of orbit/m if specified
C                                 explicitly
C      ECCEN      D              Eccentricity of orbit if specified
C                                 explicitly
C      INCLIN     D              Inclination of orbit/degree if
C                                 specified explicitly
C      RAANOD     D              Right ascension of ascending node/deg
C                                 if specified explicitly
C      APERIG     D              Argument of perigee/degree if specified
C                                 explicitly
C      MANMLY     D              Mean anomaly at 0.00 days/degrees if
C                                 specified explicitly
C      IRET       I              Status: 0 - task initialized
C                                        1 - can not open OB table
C                                        2 - no antennae to update
C                                        3 - can not open AN table
C                                        4 - can not initialize task
C                                      999 - logic error
C
C   Postconditions:
C      IRET = 0 implies
C         (INPUTS is initialized) and (SUBARR > 0) and
C         (ANTAB is initialized) and (ANTAB.VER = SUBARR) and
C         (ANTAB is closed) and
C         (SEMIMA < ERAD implies (ORBTAB is initialized) and
C                                   (ORBTAB is closed)) and
C         (1 <= OCOUNT <= length(OANTS)) and
C         (for all i, 1 <= i <= OCOUNT, OANTS(i) > 0)
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), ANTAB*(*), ORBTAB*(*)
      INTEGER   OANTS(*), OCOUNT, SUBARR
      DOUBLE PRECISION SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY
      INTEGER   IRET
C
C     Local variables:
C
C     TASKNM       Task name
C     NPARMS       Number of adverb parameters
C     AVNAME       Adverb names
C     AVTYPE       Adverb type codes
C     AVDIM        Adverb array dimensions
C
C     ANTENS       Value of ANTENNAS adverb array
C     APARM        Value of APARM adverb array
C
      CHARACTER TASKNM*6
      PARAMETER (TASKNM = 'OBTAB ')
      INTEGER   NPARMS
      PARAMETER (NPARMS = 8)
      CHARACTER AVNAME(NPARMS)*8
      INTEGER   AVTYPE(NPARMS), AVDIM(2, NPARMS)
C
      INTEGER   ANTENS(50)
      REAL      APARM(10)
C
      INTEGER   I, TYPE, DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PEARTH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'INVERS  ', 'SUBARRAY', 'ANTENNAS', 'APARM   '/
      DATA AVTYPE /OOACAR,     OOACAR,     OOAINT,     OOAINT,
     *             OOAINT,     OOAINT,     OOAINT,     OOARE/
      DATA AVDIM  /12, 1,      6, 1,        1, 1,       1, 1,
     *              1, 1,      1, 1,       50, 1,      10, 1/
C-----------------------------------------------------------------------
      CALL AV2INP (TASKNM, NPARMS, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET.EQ.0) THEN
C
C     Initialize antenna table:
C
         CALL INIATB (ANTAB, INPUTS, IRET)
         IF (IRET.EQ.0) THEN
C
C           Read the subarray number:
C
            CALL TABGET (ANTAB, 'VER', TYPE, DIM, SUBARR, CDUMMY, IRET)
            CALL CHECK ('OBTINI', 1,
     *                  ((IRET.EQ.0) .AND. (TYPE.EQ.OOAINT)
     *                   .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *                   .AND. (SUBARR.GT.0)), IRET)
            IF (IRET.NE.0) GO TO 999
C
C           Read the antenna specifications:
C
            CALL INGET (INPUTS, 'ANTENNAS', TYPE, DIM, ANTENS, CDUMMY,
     *                  IRET)
            CALL CHECK ('OBTINI', 2,
     *                  ((IRET.EQ.0) .AND. (TYPE.EQ.OOAINT)
     *                   .AND. (DIM(1).LE.50) .AND. (DIM(2).EQ.1)),
     *                  IRET)
            IF (IRET.NE.0) GO TO 999
            OCOUNT = 0
C           Inv: OCOUNT >= 0 and OANTS(1:OCOUNT) > 0
            DO 10 I = 1, 50
               IF (ANTENS(I).GT.0) THEN
                  OCOUNT = OCOUNT + 1
                  OANTS(OCOUNT) = ANTENS(I)
               END IF
   10       CONTINUE
C
C           Look-up orbiting antennae in the AN table if the user
C           has not indicated which antennae are to be updated:
C
            IF (OCOUNT.EQ.0) THEN
               CALL ORBANS (ANTAB, OANTS, OCOUNT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT, 9004) IRET
                  IRET = 3
                  OCOUNT = 0
               END IF
            END IF
C
            IF (OCOUNT.GT.0) THEN
C
C              Get user-supplied orbital elements:
C
               CALL INGET (INPUTS, 'APARM', TYPE, DIM, APARM, CDUMMY,
     *                     IRET)
               CALL CHECK ('OBTINI', 3,
     *                     ((IRET.EQ.0) .AND. (TYPE.EQ.OOARE)
     *                      .AND. (DIM(1).EQ.10)
     *                      .AND. (DIM(2).EQ.1)), IRET)
               IF (IRET.NE.0) GO TO 999
               SEMIMA = APARM(1)
               ECCEN  = APARM(2)
               INCLIN = APARM(3)
               RAANOD = APARM(4)
               APERIG = APARM(5)
               MANMLY = APARM(6)
               IF (SEMIMA.LT.ERAD) THEN
                  CALL INIOTB (ORBTAB, INPUTS, SUBARR, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT, 9001) IRET
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
               END IF
            ELSE
               MSGTXT = 'NO ANTENNAE TO CORRECT --- NOTHING TO DO'
               CALL MSGWRT (9)
               IRET = 2
            END IF
         ELSE
            WRITE (MSGTXT, 9002) IRET
            CALL MSGWRT (9)
            IRET = 3
         END IF
      ELSE
         WRITE (MSGTXT, 9003) IRET
         CALL MSGWRT (9)
         IRET = 4
      END IF
C
  999 CONTINUE
C-----------------------------------------------------------------------
 9001 FORMAT ('OBTINI: FAILED TO INITIALIZE OB TABLE (ERROR ', I4, ')')
 9002 FORMAT ('OBTINI: FAILED TO INITIALIZE AN TABLE (ERROR ', I4, ')')
 9003 FORMAT ('OBTINI: FAILED TO INITIALIZE TASK (ERROR ', I4, ')')
 9004 FORMAT ('OBTINI: FAILED TO READ ANTENNA TABLE (ERROR ', I4, ')')
      END
      SUBROUTINE OBHIST (INPUTS, ANTAB, ORBTAB, IRET)
C-----------------------------------------------------------------------
C   Update history file.
C
C   Inputs:
C      INPUTS      C*(*)      Name of INPUTS object holding adverbs
C      ANTAB       C*(*)      Name of TABLE object used for antenna
C                              table.  If the table has been modified
C                              then there should be an integer keyword,
C                              NUPDATED, giving the number of entries
C                              changed and an integer array keyword
C                              UPDATED, listing the station numbers of
C                              the modified antennae in its first
C                              NUPDATED elements.
C      ORBTAB      C*(*)      Name of TABLE object used for the orbit
C                              table.  If the table has been modified
C                              then there should be an integer keyword
C                              NUPDATED giving the number of records
C                              updated.
C
C   Output:
C      IRET        I          Status: 0 - history updated
C                                     1 - error writing history
C                                   999 - logic error
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), ANTAB*(*), ORBTAB*(*)
      INTEGER   IRET
C
C     Local variables:
C
C     AUPDTD    List of updated antennae
C     NUPDAT    Number of AN or OB table records updated
C     ENTRY     History file record buffer
C
C     NPARMS    Number of adverb values to record in history
C     AVNAME    Names of adverbs to be recorded
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   AUPDTD(MAXANT), NUPDAT
      CHARACTER ENTRY*72
      INTEGER   NPARMS
      PARAMETER (NPARMS = 8)
      CHARACTER AVNAME(NPARMS)*8
C
      INTEGER   I, TYPE, DIM(3), MSGSAV
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'INVERS  ', 'SUBARRAY', 'ANTENNAS', 'APARM   '/
C-----------------------------------------------------------------------
      CALL OHTIME (ANTAB, IRET)
      IF (IRET.EQ.0) THEN
C
C        Copy adverb values:
C
         CALL OHLIST (INPUTS, AVNAME, NPARMS, ANTAB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
         IRET = 1
      END IF
C
C     Add extra details if the information is available (suppress error
C     messages since errors are not critical here):
C
      IF (IRET.EQ.0) THEN
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OGET (ORBTAB, 'NUPDATED', TYPE, DIM, NUPDAT, CDUMMY,
     *      IRET)
         MSGSUP = MSGSAV
         IF (IRET.EQ.0) THEN
            CALL CHECK ('OBHIST', 1,
     *                  ((TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *                   .AND. (DIM(2).EQ.1)), IRET)
            IF (IRET.NE.0) GO TO 999
            WRITE (ENTRY, 1000) NUPDAT
            CALL OHWRIT (ENTRY, ORBTAB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 9000) IRET
               CALL MSGWRT (9)
               IRET = 1
            END IF
         ELSE
            IRET = 0
         END IF
      END IF
      IF (IRET.EQ.0) THEN
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OGET (ANTAB, 'NUPDATED', TYPE, DIM, NUPDAT, CDUMMY,
     *               IRET)
         MSGSUP = MSGSAV
         IF (IRET.EQ.0) THEN
            CALL CHECK ('OBHIST', 2,
     *                  ((TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *                   .OR. (DIM(2).NE.1)), IRET)
            IF (IRET.NE.0) GO TO 999
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL OGET (ANTAB, 'UPDATED', TYPE, DIM, AUPDTD, CDUMMY,
     *                  IRET)
            MSGSUP = MSGSAV
            CALL CHECK ('OBHIST', 3,
     *                  ((IRET.EQ.0) .AND. (TYPE.EQ.OOAINT)
     *                   .AND. (DIM(1).LE.MAXANT)
     *                   .AND. (DIM(2).EQ.1)), IRET)
            IF (IRET.NE.0) GO TO 999
            DO 10 I = 1, NUPDAT
               IF (IRET.EQ.0) THEN
                  WRITE (ENTRY, 1001) AUPDTD(I)
                  CALL OHWRIT (ENTRY, ANTAB, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT, 9000) IRET
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
               END IF
   10       CONTINUE
         ELSE
            IRET = 0
         END IF
      END IF
C
  999 CONTINUE
C-----------------------------------------------------------------------
 1000 FORMAT ('/ Updated ', I6, ' OB table records.')
 1001 FORMAT ('/ Added orbital elements for antenna ', I3)
 9000 FORMAT ('OBHIST: ERROR ', I4, ' UPDATING HISTORY FILE')
      END
      SUBROUTINE INIATB (ANTAB, INPUTS, IRET)
C-----------------------------------------------------------------------
C   Initialize antenna table ANTAB using adverb values from INPUTS.
C   Reformat the table to hold the six orbital elements if necessary,
C   and leave the table closed.
C
C   Inputs:
C      ANTAB    C*(*)      The name of the TABLE object used to access
C                           the antenna table.
C      INPUTS   C*(*)      The name of the INPUTS object used to access
C                           the input adverbs
C
C   Output:
C      IRET     I          Status: 0 - table initialized
C                                  1 - failed to reformat table
C                                  2 - failed to open table
C                                  3 - failed to create table object
C
C   Preconditions:
C      INPUTS.INNAME is defined
C      INPUTS.INCLASS is defined
C      INPUTS.INSEQ is defined
C      INPUTS.INDISK is defined
C      INPUTS.SUBARR is defined
C
C   Postconditions:
C      IRET = 0 implies that
C         (ANTAB is initialized) and (ANTAB refers to an existing table)
C         and (ANTAB has room for orbital elements)
C         and (ANTAB is not open)
C-----------------------------------------------------------------------
      CHARACTER ANTAB*(*), INPUTS*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     NKEY       Number of adverbs to copy to ANTAB
C     INKEY      Adverbs to copy to ANTAB
C     OUTKEY     ANKEY attributes to receive adverb values
C
C     COLDIM     Antenna table column dimensions
C
C     KEYWRD     Keywords to read from ANTAB
C     KEYVAL     Keyword value array
C     KEYLOC     Keyword value pointers
C     KEYTYP     Keyword value type codes
C
      INTEGER   NKEY
      PARAMETER (NKEY = 5)
      CHARACTER INKEY(NKEY)*8, OUTKEY(NKEY)*8, KEYWRD(1)*8
      INTEGER   COLDIM(MAXANC), KEYVAL(1), KEYLOC(1), KEYTYP(1)
C
      INTEGER   TYPE, DIM(3), NDUMMY
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA INKEY  /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'SUBARRAY'/
      DATA OUTKEY /'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    ',
     *             'VER     '/
      DATA KEYWRD /'NUMORB  '/
C-----------------------------------------------------------------------
      CALL TABCRE (ANTAB, IRET)
      IF (IRET.EQ.0) THEN
         CALL IN2OBJ (INPUTS, NKEY, INKEY, OUTKEY, ANTAB, IRET)
         CALL CHECK ('INIATB', 1, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         TYPE = OOACAR
         DIM(1) = 2
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (ANTAB, 'TBLTYPE ', TYPE, DIM, NDUMMY, 'AN', IRET)
         CALL CHECK ('INIATB', 2, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABOPN (ANTAB, 'READ', IRET)
         IF (IRET.EQ.0) THEN
C
C        Make sure that the table has the correct column dimension for
C        the ORBPRM column:
C
            CALL TABGET (ANTAB, 'COLDIM', TYPE, DIM, COLDIM, CDUMMY,
     *                   IRET)
            CALL CHECK ('INIATB', 3,
     *                  ((IRET.EQ.0) .AND. (TYPE.EQ.OOAINT)
     *                   .AND. (DIM(1).LE.MAXANC)
     *                   .AND. (DIM(2).EQ.1)), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABCLO (ANTAB, IRET)
            CALL CHECK ('INIATB', 4, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            IF (COLDIM(3).NE.6) THEN
C
C              Reformat table:
C
               CALL REFATB (ANTAB, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            ELSE
C
C              Make sure number of orbital elements is correct:
C
               CALL TABOPN (ANTAB, 'WRIT', IRET)
               IF (IRET.EQ.0) THEN
                  CALL TABKGT (ANTAB, KEYWRD, 1, KEYLOC, KEYVAL, KEYTYP,
     *                         IRET)
                  CALL CHECK ('INIATB', 5, ((IRET.EQ.0)
     *                        .AND. (KEYTYP(1).EQ.OOAINT)), IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (KEYVAL(KEYLOC(1)).NE.6) THEN
                     KEYVAL(KEYLOC(1)) = 6
                     CALL TABKPT (ANTAB, KEYWRD, 1, KEYLOC, KEYVAL,
     *                  KEYTYP,IRET)
                     CALL CHECK ('INIATB', 6, (IRET.EQ.0), IRET)
                     IF (IRET.NE.0) GO TO 999
                  END IF
                  CALL TABCLO (ANTAB, IRET)
                  CALL CHECK ('INIATB', 7, (IRET.EQ.0), IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (9)
                  IRET = 2
               END IF
            END IF
         ELSE
            WRITE (MSGTXT, 9001) IRET
            CALL MSGWRT (9)
            IRET = 2
         END IF
      ELSE
         WRITE (MSGTXT, 9002) IRET
         CALL MSGWRT (9)
         IRET = 3
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('INIATB: FAILED TO REFORMAT ANTENNA TABLE (ERROR ', I3,
     *   ')')
 9001 FORMAT ('INIATB: FAILED TO OPEN ANTENNA TABLE (ERROR ', I3, ')')
 9002 FORMAT ('INIATB: FAILED TO CREATE TABLE OBJECT (ERROR ', I3, ')')
      END
      SUBROUTINE UPDATB (ANTAB, OANTS, OCOUNT, SEMIMA, ECCEN, INCLIN,
     *                   RAANOD, APERIG, MANMLY, IRET)
C-----------------------------------------------------------------------
C   Update the orbital elements for the antennae listed in the first
C   OCOUNT elements of ANTAB and ensure that their mount type is
C   "orbiting".  Insert the number of records updated as ANTAB.NUPDATED
C   (integer attribute) and the numbers of the antennae updated as
C   ANTAB.UPDATED (integer array attribute).
C
C   Inputs:
C      ANTAB     C*(*)     The name of the TABLE object used to access
C                           the antenna table
C      OANTS     I(*)      A list of antenna numbers
C      OCOUNT    I         The number of entries in OANTS
C      SEMIMA    D         Semimajor axis of orbit/m
C      ECCEN     D         Eccentricity of orbit
C      INCLIN    D         Inclination of orbit/degrees
C      RAANOD    D         Right ascension of ascending node/degrees
C      APERIG    D         Argument of perigee/degrees
C      MANMLY    D         Mean anomaly at 0.00 days/degrees
C
C   Output:
C      IRET      I         Status: 0 - table updated
C                                  1 - can not open table
C                                999 - logic error
C
C   Preconditions:
C      ANTAB is initialized
C      ANTAB refers to an existing AN table with room for 6 orbital
C        elements
C      ANTAB is closed
C
C   Postconditions:
C      IRET = 0 implies that
C         ((ANTAB has been updated) and (ANTAB is closed)
C          and (0 <= ANTAB.NUPDATED <= MAXANT)
C          and (for all i, 1 <= i <= ANTAB.NUPDATED,
C              1 <= ANTAB.UPDATED(i) <= MAXANT))
C-----------------------------------------------------------------------
      CHARACTER ANTAB*(*)
      INTEGER   OANTS(*), OCOUNT, IRET
      DOUBLE PRECISION SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables
C
C     NUMROW       Number of rows in table
C     ANROW        Current row
C     UPDATD       List of antennae updated
C     NUPDAT       Number of antennae updated
C     MATCH        Does the current row match an antenna to be updated?
C
      INTEGER   NUMROW, ANROW, UPDATD(MAXANT), NUPDAT
      LOGICAL   MATCH
C
      INTEGER   TYPE, DIM(3), I, J, IERR
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL TABOPN (ANTAB, 'WRIT', IRET)
      IF (IRET.EQ.0) THEN
         CALL TABGET (ANTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
         CALL CHECK ('UPDATB', 1, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Close the table and reopen for reading/writing using AN table
C        specific routines (no failures are expected since the table
C        has already been opened successfully):
C
         CALL TABCLO (ANTAB, IRET)
         CALL CHECK ('UPDATB', 2, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OANINI (ANTAB, 'WRIT', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         CALL CHECK ('UPDATB', 3, (IRET.EQ.0), IRET)
C
C        Test every unflagged record in the antenna table against the
C        list of antennae to be modified and updated the records which
C        match, recording the updated antennae in UPDATD(1:NUPDAT):
C
         NUPDAT = 0
C
C        Invariant: records 1 to I have been updated and
C                   UPDATD(1:NUPDAT) is the set of updated antennae
C
         DO 20 I = 1, NUMROW
            ANROW = I
            CALL OTABAN (ANTAB, 'READ', ANROW, ANNAME, STAXYZ, ORBPRM,
     *         NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *         POLCA, POLTYB, POLAB, POLCB, IERR)
            CALL CHECK ('UPDATB', 4,
     *                  ((IERR.EQ.0) .OR. (IERR.EQ.-1)), IRET)
            IF (IRET.NE.0) GO TO 999
            IF (IERR.EQ.0) THEN
               MATCH = .FALSE.
C
C              Invariant: MATCH = NOSTA in OANTS(1:J)
C
               DO 10 J = 1, OCOUNT
                  IF (OANTS(J).EQ.NOSTA) THEN
                     MATCH = .TRUE.
                  END IF
   10          CONTINUE
C
C              MATCH = NOSTA in OANTS(1:OCOUNT)
C
               IF (MATCH) THEN
                  ANROW = I
                  MNTSTA = 2
                  ORBPRM(1) = SEMIMA
                  ORBPRM(2) = ECCEN
                  ORBPRM(3) = INCLIN
                  ORBPRM(4) = RAANOD
                  ORBPRM(5) = APERIG
                  ORBPRM(6) = MANMLY
                  CALL OTABAN (ANTAB, 'WRIT', ANROW, ANNAME, STAXYZ,
     *               ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *               POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
                  CALL CHECK ('UPDATB', 5, (IRET.EQ.0), IRET)
                  IF (IRET.NE.0) GO TO 999
                  NUPDAT = NUPDAT + 1
                  CALL CHECK ('UPDATB', 6, (NUPDAT.LE.MAXANT), IRET)
                  IF (IRET.NE.0) GO TO 999
                  UPDATD(NUPDAT) = NOSTA
               END IF
            END IF
   20    CONTINUE
         CALL TABCLO (ANTAB, IRET)
         CALL CHECK ('UPDATB', 7, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (MSGTXT, 1020) NUPDAT
         CALL MSGWRT (4)
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (ANTAB, 'NUPDATED', TYPE, DIM, NUPDAT, CDUMMY,
     *                IRET)
         CALL CHECK ('UPDATB', 8, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = MAX (1, NUPDAT)
         CALL TABPUT (ANTAB, 'UPDATED', TYPE, DIM, UPDATD, CDUMMY,
     *      IRET)
         CALL CHECK ('UPDATB', 9, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
         IRET = 1
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Updated ', I3, ' antenna table records.')
 9000 FORMAT ('UPDATB: FAILED TO OPEN AN TABLE (ERROR ', I3, ')')
      END
      SUBROUTINE ORBANS (ANTAB, OANTS, OCOUNT, IRET)
C-----------------------------------------------------------------------
C   List the orbiting antennae found in ANTAB in the first OCOUNT
C   elements of OANTS.  Identify the orbiting antennae using their
C   mount type code.
C
C   Input:
C      ANTAB     C*(*)         The TABLE object used to access the AN
C                               table.
C
C   Outputs:
C      OANTS     I*(MAXANT)    A list of orbiting antennae
C      OCOUNT    I             The number of antennae in OANTS
C      IRET      I             Status: 0 - table searched
C                                      1 - failed to open table
C                                    999 - logic error
C
C   Preconditions:
C      ANTAB is initialized
C      ANTAB is closed
C
C   Postconditions:
C      IRET = 0 implies that
C         ((ANTAB is closed) and (0 <= OCOUNT <= MAXANT)
C         and (for all i, 1 <= i <= MAXANT, 1 <= OANTS(i) <= MAXANT))
C-----------------------------------------------------------------------
      CHARACTER ANTAB*(*)
      INTEGER   OANTS(*), OCOUNT, IRET
C
C     Local variables:
C
C     NUMROW       Number of rows in antenna table
C     ANROW        Current row in antenna table
C
      INTEGER   NUMROW, ANROW
C
      INTEGER   TYPE, DIM(3), I, IERR
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL TABOPN (ANTAB, 'READ', IRET)
      IF (IRET.EQ.0) THEN
         CALL TABGET (ANTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
         CALL CHECK ('ORBANS', 1, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Close the table and reopen for reading using AN table
C        specific routines (no failures are expected since the table
C        has already been opened successfully):
C
         CALL TABCLO (ANTAB, IRET)
         CALL CHECK ('ORBANS', 2, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OANINI (ANTAB, 'READ', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         CALL CHECK ('ORBANS', 3, (IRET.EQ.0), IRET)
C
         OCOUNT = 0
C
C        Invariant: OANTS(1:OCOUNT) is a list of orbiting antennae
C                   found in rows 1 to I
C
         DO 10 I = 1, NUMROW
            ANROW = I
            CALL OTABAN (ANTAB, 'READ', ANROW, ANNAME, STAXYZ, ORBPRM,
     *         NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *         POLCA, POLTYB, POLAB, POLCB, IERR)
            CALL CHECK ('ORBANS', 4,
     *                  ((IERR.EQ.0) .OR. (IERR.EQ.-1)), IRET)
            IF (IRET.NE.0) GO TO 999
            IF (IERR.EQ.0) THEN
               IF (MNTSTA.EQ.2) THEN
                  OCOUNT = OCOUNT + 1
                  CALL CHECK ('ORBANS', 5, (OCOUNT.LE.MAXANT), IRET)
                  IF (IRET.NE.0) GO TO 999
                  OANTS(OCOUNT) = NOSTA
               END IF
            END IF
   10    CONTINUE
         CALL TABCLO (ANTAB, IRET)
         CALL CHECK ('ORBANS', 6, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
         IRET = 1
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('ORBANS: FAILED TO OPEN ANTENNA TABLE (ERROR ', I3, ')')
      END
      SUBROUTINE REFATB (ANTAB, IRET)
C-----------------------------------------------------------------------
C   Reformat ANTAB to hold 6 orbital parameters.
C
C   Inputs:
C      ANTAB   C*(*)     The name of the TABLE object used to access
C                         the antenna table.
C
C   Outputs:
C      IRET    I         Status: 0 - table reformatted
C                                1 - failed to delete scratch table
C                                2 - failed to restore AN table
C                                3 - failed to remove AN table
C                                4 - failed to create scratch table
C                                5 - failed to create TABLE object
C                                6 - failed to open AN table
C                              999 - logic error
C
C   Preconditions:
C      (ANTAB is defined) and (ANTAB exists)
C
C   Postconditions:
C      IRET = 0 implies that
C         (ANTAB.COLDIM(3) = 6) and (NUMORB keyword is set to 6)
C-----------------------------------------------------------------------
      CHARACTER ANTAB*(*)
      INTEGER   IRET
C
C     Local variables
C
C     ANTEMP      Temporary TABLE object
C     NUMROW      Number of rows in AN table
C     ANROW       Current row number
C     INVER       Input table version number
C     OUTVER      Output table version number
C
      CHARACTER ANTEMP*18
      PARAMETER (ANTEMP = 'temporary AN table')
      INTEGER   NUMROW, ANROW, INVER, OUTVER
C
      INTEGER   TYPE, DIM(3), I, J, IERR
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Create a temporary copy of ANTAB with the correct number of
C     orbital parameters, copy unflagged records to the temporary table,
C     delete the original, copy the temporary table to the original
C     table's location, and delete the temporary:
C
      CALL TABOPN (ANTAB, 'READ', IRET)
      IF (IRET.EQ.0) THEN
         CALL TABGET (ANTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
         CALL CHECK ('REFATB', 1, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABGET (ANTAB, 'VER', TYPE, DIM, INVER, CDUMMY, IRET)
         CALL CHECK ('REFATB', 2, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABCLO (ANTAB, IRET)
         CALL CHECK ('REFATB', 3, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
C
         WRITE (MSGTXT, 1000) INVER
         CALL MSGWRT (5)
C
         CALL TABCRE (ANTEMP, IRET)
         IF (IRET.EQ.0) THEN
            CALL TBCOPY (ANTAB, ANTEMP, IRET)
            CALL CHECK ('REFATB', 4, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            TYPE = OOAINT
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
            CALL TABPUT (ANTEMP, 'VER', TYPE, DIM, 0, CDUMMY, IRET)
            CALL CHECK ('REFATB', 5, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OANINI (ANTAB, 'READ', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *         SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *         XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
            CALL CHECK ('REFATB', 6, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OANINI (ANTEMP, 'WRIT', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *         SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *         XYZHAN, TFRAME, 6, NOPCAL, ANTNIF, ANFQID, IRET)
            IF (IRET.EQ.0) THEN
               CALL TABGET (ANTEMP, 'VER', TYPE, DIM, OUTVER, CDUMMY,
     *                      IRET)
               CALL CHECK ('REFATB', 7, ((IRET.EQ.0)
     *                     .AND. (TYPE.EQ.OOAINT)
     *                     .AND. (DIM(1).EQ.1)
     *                     .AND. (DIM(2).EQ.1)), IRET)
               IF (IRET.NE.0) GO TO 999
C
               WRITE (MSGTXT, 1001) INVER, OUTVER
               CALL MSGWRT (4)
C
               J = 0
               DO 10 I = 1, NUMROW
                  ANROW = I
                  CALL OTABAN (ANTAB, 'READ', ANROW, ANNAME, STAXYZ,
     *               ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *               POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
                  CALL CHECK ('REFATB', 8, ((IERR.EQ.0) .OR.
     *                                      (IERR.EQ.-1)), IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (IERR.EQ.0) THEN
                     J = J + 1
                     ANROW = J
                     CALL DFILL (6, DBLANK, ORBPRM)
                     CALL OTABAN (ANTEMP, 'WRIT', ANROW, ANNAME, STAXYZ,
     *                  ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *                  POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,IRET)
                     CALL CHECK ('REFATB', 9, (IRET.EQ.0), IRET)
                     IF (IRET.NE.0) GO TO 999
                  END IF
   10          CONTINUE
               CALL TABCLO (ANTAB, IRET)
               CALL CHECK ('REFATB', 10, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABCLO (ANTEMP, IRET)
               CALL CHECK ('REFATB', 11, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
C
               WRITE (MSGTXT, 1010) J, OUTVER
               CALL MSGWRT (4)
C
               CALL TABRMV (ANTAB, IRET)
               IF (IRET.EQ.0) THEN
C
                  WRITE (MSGTXT, 1011) INVER
                  CALL MSGWRT (4)
C
                  CALL TBLCOP (ANTEMP, ANTAB, IRET)
                  IF (IRET.EQ.0) THEN
C
                     WRITE (MSGTXT, 1012) OUTVER, INVER
                     CALL MSGWRT (4)
C
                     CALL TABZAP (ANTEMP, IRET)
                     IF (IRET.EQ.0) THEN
                        WRITE (MSGTXT, 1013) OUTVER
                        CALL MSGWRT (4)
                        WRITE (MSGTXT, 1014) INVER
                        CALL MSGWRT (5)
                     ELSE
                        WRITE (MSGTXT, 9000) IRET
                        CALL MSGWRT (9)
                        WRITE (MSGTXT, 9001) OUTVER
                        CALL MSGWRT (6)
                        IRET = 1
                     END IF
                  ELSE
                     WRITE (MSGTXT, 9002) IRET
                     CALL MSGWRT (9)
                     WRITE (MSGTXT, 9003) OUTVER, INVER
                     CALL MSGWRT (6)
                     IRET = 2
                  END IF
               ELSE
                  WRITE (MSGTXT, 9004) IRET
                  CALL MSGWRT (9)
                  WRITE (MSGTXT, 9005) OUTVER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT, 9006)
                  CALL MSGWRT (6)
                  IRET = 3
               END IF
            ELSE
               WRITE (MSGTXT, 9007) IRET
               CALL MSGWRT (9)
               IRET = 4
            END IF
         ELSE
            WRITE (MSGTXT, 9008) IRET
            CALL MSGWRT (9)
            IRET = 5
         END IF
      ELSE
         WRITE (MSGTXT, 9009) IRET
         CALL MSGWRT (9)
         IRET = 6
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reformatting AN table ', I2,
     *        ' to hold orbital elements.')
 1001 FORMAT ('Copying AN table ', I2, ' to temporary version ', I2)
 1010 FORMAT ('Copied ', I5, ' records to AN table ', I2)
 1011 FORMAT ('Removed AN table ', I2)
 1012 FORMAT ('Copied temporary AN table ', I2, ' to AN table ', I2)
 1013 FORMAT ('Removed temporary AN table ', I2)
 1014 FORMAT ('Reformatted AN table ', I2)
 9000 FORMAT ('REFATB: UNABLE TO DELETE SCRATCH TABLE (ERROR ', I3,
     *        ')')
 9001 FORMAT ('YOU MUST MANUALLY DELETE AN TABLE ', I2)
 9002 FORMAT ('REFATB: UNABLE TO COPY SCRATCH TABLE (ERROR ', I3, ')')
 9003 FORMAT ('YOU MUST MANUALLY COPY AN TABLE ', I2, ' TO ', I2,
     *        ' USING TACOP')
 9004 FORMAT ('REFATB: UNABLE TO DELETE ORIGINAL TABLE (ERROR ', I3,
     *        ')')
 9005 FORMAT ('MANUALLY DELETE SCRATCH AN TABLE ', I2,
     *        ' AND CHECK FILE')
 9006 FORMAT ('STATUS')
 9007 FORMAT ('REFATB: CAN NOT CREATE SCRATCH TABLE (ERROR ', I3, ')')
 9008 FORMAT ('REFATB: CAN NOT CREATE TABLE OBJECT (ERROR ', I3, ')')
 9009 FORMAT ('REFATB: CAN NOT OPEN AN TABLE (ERROR ', I3, ')')
      END
      SUBROUTINE INIOTB (ORBTAB, INPUTS, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Initialize orbit table ORBTAB using adverb values from INPUTS.
C
C   Inputs:
C      ORBTAB  C*(*)      The name of the TABLE object used to access
C                          the OB table.
C      INPUTS  C*(*)      The name of the INPUTS object used to access
C                          the input adverbs
C      SUBARR  I          Subarray number
C
C   Output:
C      IRET    I          Status: 0 - ORBTAB is initialized
C                                 1 - could not initialize source
C                                     look-up table
C                                 2 - could not open OB table
C                                 3 - could not create OB TABLE object
C                                 4 - could not create UVDATA object
C                               999 - logic error
C
C   Preconditions:
C      INPUTS.INNAME is defined
C      INPUTS.INCLASS is defined
C      INPUTS.INSEQ is defined
C      INPUTS.INDISK is defined
C      INPUTS.INVERS is defined
C      SUBARR > 0
C
C   Postconditions:
C      IRET = 0 implies that
C         ((ORBTAB is initialized)
C          and (ORBTAB refers to an existing table)
C          and (ORBTAB is closed)
C          and (OTAB.JD0 is Julian day number at midnight on reference
C               date)
C-----------------------------------------------------------------------
      CHARACTER ORBTAB*(*), INPUTS*(*)
      INTEGER SUBARR, IRET
C
C     Local variables
C
C     NKEY      Number of adverbs to copy to TABLE object
C     INKEY     Adverbs to copy to TABLE object
C     UVKEY     UVDATA keywords to receive adverb values
C     TABKEY    TABLE keywords to receive adverb values
C     REFDAT    Reference day as character string
C     JD0       Julian day number at 0h on reference day
C
      INTEGER   NKEY
      PARAMETER (NKEY = 5)
      CHARACTER INKEY(NKEY)*8, UVKEY(NKEY-1)*16, TABKEY(NKEY)*8,
     *   REFDAT*8, UVDATA*6
      PARAMETER (UVDATA = 'UVDATA')
      DOUBLE PRECISION JD0
C
      INTEGER   TYPE, DIM(3), NDUMMY
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA INKEY /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *            'INVERS  '/
      DATA UVKEY /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *            'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
      DATA TABKEY /'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    ',
     *             'VER     '/
C-----------------------------------------------------------------------
      CALL OUVCRE (UVDATA, IRET)
      IF (IRET.EQ.0) THEN
         CALL IN2OBJ (INPUTS, NKEY - 1, INKEY, UVKEY, UVDATA, IRET)
         CALL CHECK ('INIOTB', 1, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABCRE (ORBTAB, IRET)
         IF (IRET.EQ.0) THEN
            CALL IN2OBJ (INPUTS, NKEY, INKEY, TABKEY, ORBTAB, IRET)
            CALL CHECK ('INIOTB', 2, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            TYPE = OOACAR
            DIM(1) = 2
            DIM(2) = 1
            DIM(3) = 0
            CALL TABPUT (ORBTAB, 'TBLTYPE', TYPE, DIM, NDUMMY, 'OB',
     *                   IRET)
            CALL CHECK ('INIOTB', 3, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
C
C           Try to open the table to make sure that it exists:
C
            CALL TABOPN (ORBTAB, 'READ', IRET)
            IF (IRET.EQ.0) THEN
               CALL TABCLO (ORBTAB, IRET)
               CALL CHECK ('INIOTB', 4, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
C
C              Store the Julian day number of the reference date as
C              an attribute of ORBTAB (it will be required for eclipse
C              calculations):
C
               CALL OUVATT (UVDATA, .FALSE., IRET)
               CALL CHECK ('INIOTB', 5, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
               CALL UVDGET (UVDATA, 'DATE-OBS', TYPE, DIM, NDUMMY,
     *                      REFDAT, IRET)
               CALL CHECK ('INIOTB', 6, ((IRET.EQ.0)
     *                     .AND. (TYPE.EQ.OOACAR)
     *                     .AND. (DIM(1).EQ.8) .AND. (DIM(2).EQ.1)),
     *                     IRET)
               IF (IRET.NE.0) GO TO 999
               CALL JULDAY (REFDAT, JD0)
               TYPE = OOADP
               DIM(1) = 1
               DIM(2) = 1
               DIM(3) = 0
               CALL TABPUT (ORBTAB, 'JD0', TYPE, DIM, JD0, CDUMMY, IRET)
               CALL CHECK ('INIOTB', 7, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
C
C              Set up a look up table of source positions as a function
C              of time:
C
               CALL SETCRD (UVDATA, SUBARR, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT, 9000) IRET
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            ELSE
               WRITE (MSGTXT, 9001) IRET
               CALL MSGWRT (9)
               IRET = 2
            END IF
         ELSE
            WRITE (MSGTXT, 9002) IRET
            CALL MSGWRT (9)
            IRET = 3
         END IF
C
C        Finalize the UVDATA object which is no longer needed:
C
         CALL OUVDES (UVDATA, IRET)
         CALL CHECK ('INIOTB', 8, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         WRITE (MSGTXT, 9003) IRET
         CALL MSGWRT (9)
         IRET = 4
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('INIOTB: FAILED TO INITIALIZE SOURCE TABLE (ERROR ', I3,
     *        ')')
 9001 FORMAT ('INIOTB: FAILED TO OPEN OB TABLE (ERROR ', I3, ')')
 9002 FORMAT ('INIOTB: FAILED TO CREATE OB TABLE OBJECT (ERROR ', I3,
     *        ')')
 9003 FORMAT ('INIOTB: FAILED TO CREATE UVDATA OBJECT (ERROR ', I3,
     *        ')')
      END
      SUBROUTINE UPDOTB (ORBTAB, OANTS, OCOUNT, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Fill in missing quantities in ORBTAB for records with antenna numbers
C   matching the first OCOUNT entries in OANTS and with subarray numbers
C   matching subarray (interpret zero antenna numbers and zero subarray
C   numbers as matching).  Calculate the mean orbital elements from the
C   matching records and record them as attributes of ORBTAB.  Also
C   attach the total number of records altered as an attribute of ORBTAB.
C
C   If a missing antenna number is to be inserted use the first element
C   of OANTS.
C
C   Inputs:
C      ORBTAB      C*(*)       The name of the TABLE object used to
C                              access the OB table
C      OANTS      I(*)        List of antenna numbers to update
C      OCOUNT     I           Number of entries in OANTS
C      SUBARR     I           Subarray to which antennae belong
C
C   Output:
C      IRET       I           Status: 0 - table updated
C                                     1 - no OB records selected
C                                     2 - can not open OB table
C                                   999 - logic error
C
C   Preconditions:
C      ORBTAB is initialized
C      ORBTAB refers to an existing OB table
C      ORBTAB is closed
C      1 <= OCOUNT <= MAXANT
C      for all i, 1 <= i <= OCOUNT, 1 <= OANTS(i) <= MAXANT
C      SUBARR > 0
C
C   Postconditions:
C      IRET = 0 implies that
C         ((ORBTAB has been updated) and (ORBTAB is closed)
C          and (ORBTAB.NUPDATED = number of records modified)
C          and (ORBTAB.SEMIMA = semimajor axis/m)
C          and (ORBTAB.ECCEN  = eccentricity)
C          and (ORBTAB.INCLIN = inclination/deg)
C          and (ORBTAB.RAANOD = RA of ascending node/deg)
C          and (ORBTAB.APERIG = argument of perigee/deg)
C          and (ORBTAB.MANMLY = mean anomaly at 0.0 days/deg))
C-----------------------------------------------------------------------
      CHARACTER ORBTAB*(*)
      INTEGER   OANTS(*), OCOUNT, SUBARR, IRET
C
C     Local variables:
C
C     COUNT     Number of OB table entries processed
C     SUMA      Sum of semimajor axis values (m)
C     SUMECC    Sum of eccentricity values
C     SUMINC    Sum of inclinations (radians)
C     SUMRN     Sum of RA of ascending node values (radians)
C     SUMAP     Sum of arguments of perigee (radians)
C     SUMM      Sum of mean anomalies (radians)
C
C     NUMROW    Number of rows in OB table
C     OBROW     Current row in OB table
C     MATCH     Does current row match antenna selection criteria
C     NOSTA     Station number from OB table
C     OBSUB     Subarray from OB table
C     TIME      Time of OB table entry
C     POS       Position vector from OB table
C     VEL       Velocity vector from OB table
C     SUNANG    Sun angle values from OB table
C     ECLIPS    Eclipse parameters from OB table
C     ORIENT    Orientation angle from OB table
C
C     SEMIMA    Semimajor axis derived from OB table (m)
C     ECCEN     Eccentricity derived from OB table
C     INCLIN    Inclination derived from OB table (radians)
C     RAANOD    RA of ascending node derived from OB table (radians)
C     APERIG    Argument of perigee derived from OB table (radians)
C     MANMLY    Mean anomaly derived from OB table (radians)
C
C     RANODT    RA of ascending node at reference time (radians)
C     APERIT    Argument of perigee at reference time (radians)
C     MANMLT    Mean anomaly at reference time (radians)
C
C     RA        RA of current source (radians)
C     DEC       Declination of current souce (radians)
C     SOLRA     RA of sun (radians)
C     SOLDEC    Declination of sun (radians)
C     JD0       Julian day number at reference time
C
      DOUBLE PRECISION SUMA, SUMECC, SUMINC, SUMRN, SUMAP, SUMM, TIME,
     *                 POS(3), VEL(3), SEMIMA, ECCEN, INCLIN, RAANOD,
     *                 APERIG, MANMLY, RANODT, APERIT, MANMLT, RA, DEC,
     *                 SOLRA, SOLDEC, JD0
      REAL             SUNANG(3), ECLIPS(4), ORIENT
      INTEGER          COUNT, NUMROW, NOSTA, OBSUB, OBROW
      LOGICAL          MATCH
C
      INTEGER          I, J, TYPE, DIM(3)
      CHARACTER        CDUMMY
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PEARTH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL OOBINI (ORBTAB, 'WRIT', OBROW, IRET)
      IF (IRET.EQ.0) THEN
C
C        Initialize sums for averaging:
C
         COUNT  = 0
         SUMA   = 0.0D0
         SUMECC = 0.0D0
         SUMINC = 0.0D0
         SUMRN  = 0.0D0
         SUMAP  = 0.0D0
         SUMM   = 0.0D0
C
         CALL TABGET (ORBTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
         CALL CHECK ('UPDOTB', 1, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOAINT) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABGET (ORBTAB, 'JD0', TYPE, DIM, JD0, CDUMMY, IRET)
         CALL CHECK ('UPDOTB', 2, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOADP) .AND. (DIM(1).EQ.1)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
C
         DO 20 I = 1, NUMROW
C
            IF (MOD(I, 500).EQ.0) THEN
               WRITE (MSGTXT, 1000) I
               CALL MSGWRT (5)
            END IF
C
            OBROW = I
            CALL OTABOB (ORBTAB, 'READ', OBROW, NOSTA, OBSUB, TIME, POS,
     *                   VEL, SUNANG, ECLIPS, ORIENT, IRET)
            CALL CHECK ('UPDOTB', 3, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
C
C           Fill in default values if the antenna number or subarray
C           field is blank:
C
            IF (NOSTA.LE.0) THEN
               NOSTA = OANTS(1)
            END IF
            IF (OBSUB.LE.0) THEN
               OBSUB = SUBARR
            END IF
C
C           Check whether the current record is wanted:
C
            MATCH = .FALSE.
            DO 10 J = 1, OCOUNT
               IF (NOSTA.EQ.OANTS(J)) THEN
                  MATCH = .TRUE.
               END IF
   10       CONTINUE
            IF (MATCH .AND. (OBSUB.EQ.SUBARR)) THEN
C
C              Fill in the sun angle parameters if the current pointing
C              can be determined:
C
               CALL SUNPOS (JD0 + TIME, SOLRA, SOLDEC)
               CALL GETCRD (TIME, RA, DEC)
               SUNANG(2) = FBLANK
               SUNANG(3) = FBLANK
               IF ((RA.NE.DBLANK) .AND. (DEC.NE.DBLANK)) THEN
                  SUNANG(1) = RAD2DG * ACOS (COS (DEC) * COS (SOLDEC)
     *                                       * COS (SOLRA - RA)
     *                                       + SIN (DEC) * SIN (SOLDEC))
                  ORIENT = RAD2DG * ATAN2 (COS (SOLDEC)
     *                                     * SIN (SOLRA - RA),
     *                                     -COS (SOLDEC) * SIN (DEC)
     *                                     * COS (SOLRA - RA)
     *                                     + COS (DEC) * SIN (SOLDEC))
               ELSE
                  SUNANG(1) = FBLANK
                  ORIENT    = FBLANK
               END IF
C
C              Calculate orbital elements and use them to find eclipses
C              up to one orbit before this time:
C
               CALL VEC2OE (POS, VEL, SEMIMA, ECCEN, INCLIN, RAANOD,
     *                      APERIG, MANMLY, IRET)
               CALL CHECK ('UPDOTB', 4, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Debugging note: SUNECL begins
C                                       by calculating the position and
C                                       velocity from the orbital
C                                       elements --- this should
C                                       reproduce the original POS and
C                                       VEL apart from rounding errors.
C                                       This can be used to check the
C                                       orbit calculations
               CALL SUNECL (JD0 + TIME, SEMIMA, ECCEN, INCLIN,
     *                      RAANOD, APERIG, MANMLY, ECLIPS, 60.0D0)
C
C              Propagate the orbital elements to the reference time
C              and remove 2 * pi ambiguities:
C
               CALL PROPOE (-DBLE(TIME), SEMIMA, ECCEN, INCLIN,
     *                      RAANOD, APERIG, MANMLY, RANODT, APERIT,
     *                      MANMLT)
               IF (COUNT.GT.0) THEN
                  IF ((RANODT - SUMRN / COUNT).GT.PI) THEN
                     RANODT = RANODT - TWOPI
                  ELSE IF ((RANODT - SUMRN / COUNT).LT.-PI) THEN
                     RANODT = RANODT + TWOPI
                  END IF
                  IF ((APERIT - SUMAP / COUNT).GT.PI) THEN
                     APERIT = APERIT - TWOPI
                  ELSE IF ((APERIT - SUMAP / COUNT).LT.-PI) THEN
                     APERIT = APERIT + TWOPI
                  END IF
                  IF ((MANMLT - SUMM / COUNT).GT.PI) THEN
                     MANMLT = MANMLT - TWOPI
                  ELSE IF ((MANMLT - SUMM / COUNT).LT.-PI) THEN
                     MANMLT = MANMLT + TWOPI
                  END IF
               END IF
C
C              Update accumulators:
C
               SUMA   = SUMA + SEMIMA
               SUMECC = SUMECC + ECCEN
               SUMINC = SUMINC + INCLIN
               SUMRN  = SUMRN + RANODT
               SUMAP  = SUMAP + APERIT
               SUMM   = SUMM  + MANMLT
               COUNT  = COUNT + 1
C
C              Write out updated table record:
C
               OBROW = I
               CALL OTABOB (ORBTAB, 'WRIT', OBROW, NOSTA, OBSUB, TIME,
     *                      POS, VEL, SUNANG, ECLIPS, ORIENT, IRET)
               CALL CHECK ('UPDOTB', 5, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
            END IF
   20    CONTINUE
C
         CALL OTABOB (ORBTAB, 'CLOS', OBROW, NOSTA, OBSUB, TIME,
     *                POS, VEL, SUNANG, ECLIPS, ORIENT, IRET)
         CALL CHECK ('UPDOTB', 6, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
C
         IF (COUNT.GT.0) THEN
C
C           Calculate mean orbital elements and make sure that the
C           angles lie in their conventional ranges:
C
            SEMIMA = SUMA / COUNT
            ECCEN  = SUMECC / COUNT
            INCLIN = RAD2DG * SUMINC / COUNT
            RAANOD = RAD2DG * SUMRN / COUNT
            IF (RAANOD.LT.0.0) THEN
               RAANOD = RAANOD + 360.0D0
            ELSE IF (RAANOD.GE.360.0) THEN
               RAANOD = RAANOD - 360.0D0
            END IF
            APERIG = RAD2DG * SUMAP / COUNT
            IF (APERIG.LT.0.0) THEN
               APERIG = APERIG + 360.0D0
            ELSE IF (APERIG.GE.360.0) THEN
               APERIG = APERIG - 360.0D0
            END IF
            MANMLY = RAD2DG * SUMM / COUNT
   30       IF (MANMLY.LT.0.0) THEN
               MANMLY = MANMLY + 360.0D0
               GO TO 30
            ELSE IF (MANMLY.GE.360.0) THEN
               MANMLY = MANMLY - 360.0D0
               GO TO 30
            END IF
C
C           Record the orbital elements and the number of records
C           updated:
C
            TYPE = OOAINT
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
            CALL TABPUT (ORBTAB, 'NUPDATED', TYPE, DIM, COUNT, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 6, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            TYPE = OOADP
            CALL TABPUT (ORBTAB, 'SEMIMA', TYPE, DIM, SEMIMA, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 7, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABPUT (ORBTAB, 'ECCEN', TYPE, DIM, ECCEN, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 8, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABPUT (ORBTAB, 'INCLIN', TYPE, DIM, INCLIN, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 9, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABPUT (ORBTAB, 'RAANOD', TYPE, DIM, RAANOD, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 10, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABPUT (ORBTAB, 'APERIG', TYPE, DIM, APERIG, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 11, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABPUT (ORBTAB, 'MANMLY', TYPE, DIM, MANMLY, CDUMMY,
     *                   IRET)
            CALL CHECK ('UPDOTB', 12, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            WRITE (MSGTXT, 1030) COUNT
            CALL MSGWRT (5)
         ELSE
            MSGTXT = 'UPDOBT: NO RECORDS SELECTED FROM OB TABLE'
            CALL MSGWRT (9)
            MSGTXT = 'UPDOTB: CHECK ANTENNAS AND SUBARRAY ADVERBS'
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
         IRET = 2
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing orbit record number ', I6)
 1030 FORMAT ('Updated ', I6, ' orbit table records')
 9000 FORMAT ('UPDOTB: FAILED TO OPEN OB TABLE (ERROR ', I3, ')')
      END
      SUBROUTINE OELEM (ORBTAB, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                  MANMLY, IRET)
C-----------------------------------------------------------------------
C   Recover previously calculated orbital elements from ORBTAB.
C
C   Inputs:
C      ORBTAB    C*(*)      The name of the TABLE object used to access
C                            the OB table
C
C   Outputs:
C      SEMIMA    D         Semimajor axis of orbit/m
C      ECCEN     D         Eccentricity of orbit
C      INCLIN    D         Inclination of orbit/degrees
C      RAANOD    D         Right ascension of ascending node/degrees
C      APERIG    D         Argument of perigee/degrees
C      MANMLY    D         Mean anomaly at 0.00 days/degrees
C      IRET      I         Status: 0 - elements recovered
C
C   Preconditions:
C      ORBTAB.SEMIMA holds semimajor axis in metres
C      ORBTAB.ECCEN  holds eccentricity
C      ORBTAB.INCLIN holds inclination in degrees
C      ORBTAB.RAANOD holds RA of ascending node in degrees
C      ORBTAB.APERIG holds argument of perigee in degrees
C      ORBTAB.MANMLY holds mean anomaly at 0.00 days in degrees
C-----------------------------------------------------------------------
      CHARACTER ORBTAB*(*)
      DOUBLE PRECISION SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY
      INTEGER   IRET
C
      INTEGER   TYPE, DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      CALL TABGET (ORBTAB, 'SEMIMA', TYPE, DIM, SEMIMA, CDUMMY, IRET)
      CALL CHECK ('OELEM', 1, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (SEMIMA.GT.0.0D0)), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (ORBTAB, 'ECCEN', TYPE, DIM, ECCEN, CDUMMY, IRET)
      CALL CHECK ('OELEM', 2, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (ECCEN.GT.0.0D0) .AND. (ECCEN.LT.1.0D0)),
     *             IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (ORBTAB, 'INCLIN', TYPE, DIM, INCLIN, CDUMMY, IRET)
      CALL CHECK ('OELEM', 3, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (INCLIN.GE.-90.0D0)
     *            .AND. (INCLIN.LE.+90.0D0)),
     *             IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (ORBTAB, 'RAANOD', TYPE, DIM, RAANOD, CDUMMY, IRET)
      CALL CHECK ('OELEM', 4, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (RAANOD.GE.0.0D0)
     *            .AND. (RAANOD.LE.360.0D0)),
     *             IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (ORBTAB, 'APERIG', TYPE, DIM, APERIG, CDUMMY, IRET)
      CALL CHECK ('OELEM', 5, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (APERIG.GE.0.0D0)
     *            .AND. (APERIG.LE.360.0D0)),
     *             IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABGET (ORBTAB, 'MANMLY', TYPE, DIM, MANMLY, CDUMMY, IRET)
      CALL CHECK ('OELEM', 6, ((IRET.EQ.0) .AND. (TYPE.EQ.OOADP)
     *            .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)
     *            .AND. (MANMLY.GE.0.0D0)
     *            .AND. (MANMLY.LE.360.0D0)),
     *             IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
      END
      SUBROUTINE SETCRD (UVDATA, SUBARR, IRET)
C-----------------------------------------------------------------------
C   Create and initialize a look-up table of source positions.
C
C   Inputs:
C     UVDATA     C*(*)      The name of the UVDATA object
C     SUBARR     I          The desired subarray
C
C   Outputs:
C     IRET       I          Status: 0 - look-up table initialized
C                                   1 - too many pointings
C                                   2 - can not open NX table
C                                   3 - can not open SU table
C                                 999 - logic error
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER   SUBARR, IRET
C
C     Local variables:
C
C     IDXTAB        Name of TABLE object used for index table
C     SRCTAB        Name of TABLE object used for source table
C     IDXROW        Current row in index table
C     NUMROW        Number of rows in index table
C
C     TIME         Centre time of index record
C     DTIME        Duration of index record
C     IDSOUR       Source ID number
C     VSTART       Starting visibility
C     VEND         Ending visibility
C     SUB          Subarray number for index record
C     FREQID       Frequency ID for index record
C
C     CRVAL        Coordinate reference values
C
      CHARACTER IDXTAB*6, SRCTAB*6
      PARAMETER (IDXTAB = 'IDXTAB')
      PARAMETER (SRCTAB = 'SRCTAB')
      INTEGER   IDXROW, NUMROW, IDSOUR, VSTART, VEND, SUB, FREQID
      REAL      TIME, DTIME
      DOUBLE PRECISION CRVAL(16)
C
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR, JLOCD,
     *   JLOCIF, INCS, INCF, INCIF
C
      INTEGER   IERR, TYPE, DIM(3), I
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'POINT.INC'
C-----------------------------------------------------------------------
      CALL OUVATT (UVDATA, .FALSE., IRET)
      CALL CHECK ('SETCRD', 1, (IRET.EQ.0), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDPNT (UVDATA, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
      CALL CHECK ('SETCRD', 2, (IRET.EQ.0), IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Look up pointings in the index table if the input file is a
C     multisource file otherwise use header coordinates:
C
      IF (ILOCSU.GT.0) THEN
         CALL UV2TAB (UVDATA, SRCTAB, 'SU', 1, IRET)
         CALL CHECK ('SETCRD', 3, (IRET.EQ.0), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABOPN (SRCTAB, 'READ', IRET)
         IF (IRET.EQ.0) THEN
C
C           Close the table since SOUNFO will be used to access it:
C
            CALL TABCLO (SRCTAB, IRET)
            CALL CHECK ('SETCRD', 4, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
C
            CALL UV2TAB (UVDATA, IDXTAB, 'NX', 1, IRET)
            CALL CHECK ('SETCRD', 5, (IRET.EQ.0), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ONXINI (IDXTAB, 'READ', IDXROW, IRET)
            IF (IRET.EQ.0) THEN
               CALL TABGET (IDXTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY,
     *                      IRET)
               CALL CHECK ('SETCRD', 6, ((IRET.EQ.0)
     *                     .AND. (TYPE.EQ.OOAINT)
     *                     .AND. (DIM(1).EQ.1) .AND. (DIM(2).EQ.1)),
     *                     IRET)
               IF (IRET.NE.0) GO TO 999
C
C           Scan the table for entries relevent to the current subarray
C           and add them to the look-up table:
C                                       [Should FREQID by checked too?]
C
               NPOINT = 0
               DO 10 I = 1, NUMROW
                  IF (IRET.EQ.0) THEN
                     IDXROW = I
                     CALL OTABNX (IDXTAB, 'READ', IDXROW, TIME, DTIME,
     *                            IDSOUR, SUB, VSTART, VEND, FREQID,
     *                            IERR)
                     CALL CHECK ('SETCRD', 7,
     *                           ((IERR.EQ.0) .OR. (IERR.EQ.-1)),
     *                           IRET)
                     IF (IRET.NE.0) GO TO 999
                     IF (IERR.EQ.0) THEN
                        IF ((SUB.EQ.SUBARR)
     *                      .AND. (NPOINT.LT.MAXPNT)) THEN
                           NPOINT = NPOINT + 1
                           STIME(NPOINT) = TIME - DTIME
                           ETIME(NPOINT) = TIME + DTIME
                           CALL SOUNFO (SRCTAB, IDSOUR, 'RAEPO', TYPE,
     *                                  DIM, RAS(NPOINT), CDUMMY, IRET)
                           CALL CHECK ('SETCRD', 8, ((IRET.EQ.0)
     *                                 .AND. (TYPE.EQ.OOADP)
     *                                 .AND. (DIM(1).EQ.1)
     *                                 .AND. (DIM(2).EQ.1)), IRET)
                           IF (IRET.NE.0) GO TO 999
                           CALL SOUNFO (SRCTAB, IDSOUR, 'DECEPO', TYPE,
     *                                  DIM, DECS(NPOINT), CDUMMY, IRET)
                           CALL CHECK ('SETCRD', 9, ((IRET.EQ.0)
     *                                 .AND. (TYPE.EQ.OOADP)
     *                                 .AND. (DIM(1).EQ.1)
     *                                 .AND. (DIM(2).EQ.1)), IRET)
                           IF (IRET.NE.0) GO TO 999
                           RAS(NPOINT)  = DG2RAD * RAS(NPOINT)
                           DECS(NPOINT) = DG2RAD * DECS(NPOINT)
                        ELSE IF ((SUB.EQ.SUBARR)
     *                           .AND. (NPOINT.EQ.MAXPNT)) THEN
                           WRITE (MSGTXT, 9000) MAXPNT
                           CALL MSGWRT (9)
                           IRET = 1
                        END IF
                     END IF
                  END IF
   10          CONTINUE
               CALL OTABNX (IDXTAB, 'CLOS', IDXROW, TIME, DTIME,
     *                      IDSOUR, SUB, VSTART, VEND, FREQID, IRET)
               CALL CHECK ('SETCRD', 10, (IRET.EQ.0), IRET)
               IF (IRET.NE.0) GO TO 999
            ELSE
               WRITE (MSGTXT, 9001) IRET
               CALL MSGWRT (9)
               IRET = 2
            END IF
         ELSE
            WRITE (MSGTXT, 9002) IRET
            CALL MSGWRT (9)
            IRET = 3
         END IF
      ELSE
         CALL UVDGET (UVDATA, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
         CALL CHECK ('SETCRD', 11, ((IRET.EQ.0)
     *               .AND. (TYPE.EQ.OOADP) .AND. (DIM(1).LE.16)
     *               .AND. (DIM(2).EQ.1)), IRET)
         IF (IRET.NE.0) GO TO 999
         NPOINT = 1
         STIME(1) = -999.0D0
         ETIME(1) = +999.0D0
         CALL CHECK ('SETCRD', 12,
     *               ((JLOCR.GE.1) .AND. (JLOCR.LE.16)), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL CHECK ('SETCRD', 13,
     *               ((JLOCD.GE.1) .AND. (JLOCD.LE.16)), IRET)
         IF (IRET.NE.0) GO TO 999
         RAS(1)   = DG2RAD * CRVAL(JLOCR)
         DECS(1)  = DG2RAD * CRVAL(JLOCD)
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('SETCRD: EXCEEDED SIZE OF LOOK-UP TABLE (', I4,
     *        ' ENTRIES)')
 9001 FORMAT ('SETCRD: FAILED TO OPEN INDEX TABLE (ERROR ', I2, ')')
 9002 FORMAT ('SETCRD: FAILED TO OPEN SOURCE TABLE (ERROR ', I2, ')')
      END
      SUBROUTINE GETCRD (TIME, RA, DEC)
C-----------------------------------------------------------------------
C   Look up source coordinates at TIME.  Set the RA and declination
C   to DBLANK if the source can not be determined.
C
C   Inputs:
C      TIME     D      Time (days)
C
C   Output:
C      RA       D      Right ascension at standard equinox (radians)
C      DEC      D      Declination at standard equinox (radians)
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME, RA, DEC
C
      INTEGER   I
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'POINT.INC'
C-----------------------------------------------------------------------
      RA  = DBLANK
      DEC = DBLANK
      DO 10 I = 1, NPOINT
         IF ((TIME.GE.STIME(I)) .AND. (TIME.LE.ETIME(I))) THEN
            RA  = RAS(I)
            DEC = DECS(I)
         END IF
   10 CONTINUE
      END
      SUBROUTINE SUNECL (JD, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                   MANMLY, ECLIPS, RESOLN)
C-----------------------------------------------------------------------
C   Fill in the Earth eclipse parameters for Julian day number JD.  The
C   time since last entering or last leaving the Earth's shadow should
C   be set to FBLANK if no eclipse occurred on the last orbit.
C
C   Inputs:
C      JD        D      Julian day number at which eclipse times are to
C                       be calculated
C      SEMIMA    D      Semimajor axis of orbit (m)
C      ECCEN     D      Eccentricity of orbit
C      INCLIN    D      Inclination of orbit (radians)
C      RAANOD    D      RA of ascending node at JD (radians)
C      APERIG    D      Argument of perigee at JD (radians)
C      MANMLY    D      Mean anomaly at JD (radians)
C      RESOLN    D      Resolution of eclipse times (seconds)
C
C   Outputs:
C      ECLIPS    R(4)   1: time since entering Earth's shadow (days)
C                       2: time since leaving Earth's shadow (days)
C                       3: FBLANK
C                       4: FBLANK
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                 MANMLY, RESOLN
      REAL             ECLIPS(4)
C
C     Local variables
C
C     PERIOD      Orbital period (days)
C     RESD        Resolution (days)
C     DT          Time shift (days)
C     SOLRA       Solar RA at DT (radians)
C     SOLDEC      Solar declination at DT (radians)
C     RANODT      RA of ascending node at DT (radians)
C     APERIT      Argument of perigee at DT (radians)
C     MANMLY      Mean anomaly at DT (radians)
C     POS         Position vector at DT (m)
C     VEL         Velocity vector at DT (m s**-1)
C     R           Length of radius vector
C     ANGLE       Angle between radious vector and sun (radians)
C     EANGLE      Half angular size of the Earth at the spacecraft (rad)
C     ISECL       Is spacecraft in eclipse at DT?
C     WASECL      Was spacecraft eclipsed at previous DT?
C
      DOUBLE PRECISION PERIOD, RESD, DT, SOLRA, SOLDEC, RANODT, APERIT,
     *                 MANMLT, POS(3), VEL(3), ANGLE, EANGLE, R
      LOGICAL          ISECL, WASECL
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PEARTH.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL RFILL (4, FBLANK, ECLIPS)
      PERIOD = (TWOPI * SQRT (SEMIMA ** 3 / EMU)) / (24.0D0 * 3600.0D0)
      RESD   = RESOLN / (24.0D0 * 3600.0)
C
C     Iterate over the previous orbit, noting when the spacecraft
C     enters and leaves shadow.  It is assumed that all coordinates are
C     referred to FK5 but the errors are probably tolerable if other
C     reference frames are used.
C
      CALL SUNPOS (JD, SOLRA, SOLDEC)
      CALL OE2VEC (SEMIMA, ECCEN, INCLIN, RAANOD, APERIG, MANMLY, POS,
     *             VEL)
      R = SQRT (POS(1) ** 2 + POS(2) ** 2 + POS(3) ** 2)
      EANGLE = ATAN (ERAD / R)
      ANGLE  = ACOS (-(POS(1) * COS (SOLDEC) * COS (SOLRA)
     *                 + POS(2) * COS (SOLDEC) * SIN (SOLRA)
     *                 + POS(3) * SIN (SOLDEC)) / R)
      WASECL = ANGLE.LE.EANGLE
      DT = RESD
   10 IF (DT.LE.PERIOD) THEN
         CALL SUNPOS (JD - DT, SOLRA, SOLDEC)
         CALL PROPOE (-DT, SEMIMA, ECCEN, INCLIN, RAANOD, APERIG,
     *                MANMLY, RANODT, APERIT, MANMLT)
         CALL OE2VEC (SEMIMA, ECCEN, INCLIN, RANODT, APERIT, MANMLT,
     *                POS, VEL)
         R = SQRT (POS(1) ** 2 + POS(2) ** 2 + POS(3) ** 2)
         EANGLE = ATAN (ERAD / R)
         ANGLE  = ACOS (-(POS(1) * COS (SOLDEC) * COS (SOLRA)
     *                    + POS(2) * COS (SOLDEC) * SIN (SOLRA)
     *                    + POS(3) * SIN (SOLDEC)) / R)
         ISECL = ANGLE.LE.EANGLE
C                                       Note that time is running
C                                       backwards in this loop
         IF (ISECL .AND. (.NOT. WASECL)) THEN
            ECLIPS(2) = DT
         ELSE IF ((.NOT. ISECL) .AND. WASECL) THEN
            ECLIPS(1) = DT
         END IF
         WASECL = ISECL
         DT = DT + RESD
         GO TO 10
      END IF
C
      END
      SUBROUTINE SUNPOS (JD, RA, DEC)
C-----------------------------------------------------------------------
C   Calculate the position of the sun on Julian day number JD in the
C   FK5 reference frame.
C
C   This is a low accuracy routine, good to about 1 minute of arc.
C
C   Inputs:
C      JD          D             Julian day number
C
C   Outputs:
C      RA          D             RA of sun (radians)
C      DEC         D             Declination of sun (radians)
C
C   References:
C      "Astronomical Algorithms", Jean Meeus, Wilmann-Bell 1991
C      (Section 24)
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, RA, DEC
C
C     Local variables:
C
C     JD0     Julian day number of epoch J2000.0
C     JCENT   Days in Julian century
C     OB2000  Obliquity of the ecliptic at J2000.0 (radians)
C     T       Time from J2000.0 (centuries)
C     L0      Geometric mean longitude (radians)
C     M       Mean anomaly (radians)
C     ECCEN   Eccentricity of the Earth's orbit
C     C       Equation of centre (radians)
C     TLONG   True longitude (radians)
C     L2000   Longitude at J2000.0 (radians)
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION JD0, JCENT, OB2000
      PARAMETER (JD0 = 2451545.0D0)
      PARAMETER (JCENT = 36525.0D0)
      PARAMETER (OB2000 = DG2RAD * 23.43929111)
      DOUBLE PRECISION T, L0, M, ECCEN, C, TLONG, L2000
C-----------------------------------------------------------------------
      T = (JD - JD0) / JCENT
      L0 = DG2RAD * (((0.0003032 * T) + 36000.76983) * T + 280.46645)
      M = DG2RAD * ((((-0.00000048 * T) - 0.0001559) * T
     *               + 35999.05030) * T + 357.52910)
      ECCEN = ((-0.0000001236 * T) - 0.000042037) * T + 0.016708617
      C = DG2RAD * ((((-0.000014 * T) - 0.004817) * T + 1.914600)
     *              * SIN (M)
     *              + ((-0.000101 * T) + 0.019993) * SIN (2.0D0 * M)
     *              + 0.000290 * SIN (3.0D0 * M))
      TLONG = L0 + C
      L2000 = TLONG - DG2RAD * 1.397D0 * T
      RA = ATAN2 (COS (OB2000) * SIN (L2000), COS (L2000))
      DEC = ASIN (SIN (OB2000) * SIN (L2000))
      END
