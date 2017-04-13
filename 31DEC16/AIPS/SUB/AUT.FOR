      SUBROUTINE AUT (BRANCH)
C-----------------------------------------------------------------------
C! site-specific test verbs
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C   AUT is a dummy routine for testing new verbs.
C   BRANCH = 1  verb T1VERB
C            2       T2VERB
C            3       T3VERB
C            4       T4VERB
C            5       T5VERB
C            6       T6VERB
C            7       T7VERB
C            8       T8VERB
C            9       T9VERB
C   Adverb values are found by a call to ADVERB and are set by a call to
C   ADVRBS.  The 2 have the same call sequence:
C      Adverb name   C*(*)
C      Adverb type   'C', 'R', 'I'
C      Number of values total
C      Number of characters in string
C      Integer value array
C      Real value array
C      Character value array
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, CDUM*1, INNAME*12, INCLAS*6, INTYPE*2
      INTEGER   POTERR, IDUM, IVOL, SEQNO, USID
      REAL      RDUM
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGNAM /'AUT '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.9)) GO TO 999
      POTERR = 31
C                                       sample fetching of adverbs
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, INNAME)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, INCLAS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INTYPE', 'C', 1, 2, IDUM, RDUM, INTYPE)
      IF (ERRNUM.NE.0) GO TO 980
      USID = NLUSER
C
      GO TO (100, 200, 300, 400, 500, 600, 700, 800, 900), BRANCH
C-----------------------------------------------------------------------
C                                       T1VERB
C-----------------------------------------------------------------------
 100  GO TO 980
C-----------------------------------------------------------------------
C                                       T2VERB
C-----------------------------------------------------------------------
 200  GO TO 980
C-----------------------------------------------------------------------
C                                       T3VERB
C-----------------------------------------------------------------------
 300  GO TO 980
C-----------------------------------------------------------------------
C                                       T4VERB
C-----------------------------------------------------------------------
 400  GO TO 980
C-----------------------------------------------------------------------
C                                       T5VERB
C-----------------------------------------------------------------------
 500  GO TO 980
C-----------------------------------------------------------------------
C                                       T6VERB
C-----------------------------------------------------------------------
 600  GO TO 980
C-----------------------------------------------------------------------
C                                       T7VERB
C-----------------------------------------------------------------------
 700  GO TO 980
C-----------------------------------------------------------------------
C                                       T8VERB
C-----------------------------------------------------------------------
 800  GO TO 980
C-----------------------------------------------------------------------
C                                       T9VERB
C-----------------------------------------------------------------------
 900  GO TO 980
C-----------------------------------------------------------------------
C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
