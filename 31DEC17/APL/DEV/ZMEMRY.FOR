       SUBROUTINE ZMEMRY (OPCODE, NAME, KWORDS, NBASE, OFFSET, IERR)
C-----------------------------------------------------------------------
C! initialize and manage dynamic memory allocations
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 2005, 2009, 2012
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
C   Allocate and free dynamic memory sections
C   Inputs:
C      OPCODE   C*4    'INIT' : init all pointers
C                      'GET ' : get one memory segment
C                      'FREE' : free one meory segment
C                      'FRAL' : free all NAME's segments
C                      'QUIT' : free all segments silently
C      NAME     C*6    Subroutine name
C      KWORDS   I      Kilo Number of INTEGER words requested
C      NBASE    I(*)   Base address: OFFSET will be an index relative to
C                      NBASE
C   In/out:
C      OFFSET   I      first index into array NBASE allocated
C   Output:
C      IERR     I      Error code: 0 okay,
C                         1 not found (FREE, FRAL, QUIT)
C                         2 tables full (GET)
C                         3 malloc error (GET)
C                         4 free error (FREE, FRAL, QUIT)
C                         5 opcode error
C   Generic version - calls ZMEMR2
C-----------------------------------------------------------------------
      CHARACTER OPCODE*(*), NAME*(*)
      INTEGER   KWORDS, NBASE(*), IERR
      LONGINT   OFFSET
C
      INTEGER   TABSIZ
      PARAMETER (TABSIZ = 50)
      INTEGER   NWDS(TABSIZ), I, J, N, M
      LONGINT   OFFST(TABSIZ), ADDR(TABSIZ)
      CHARACTER PNAME(TABSIZ)*6
      INCLUDE 'INCS:DMSG.INC'
      SAVE      NWDS, ADDR, OFFST, PNAME
C-----------------------------------------------------------------------
      IERR = 0
C                                       init
      IF (OPCODE(:4).EQ.'INIT') THEN
         DO 10 I = 1,TABSIZ
            NWDS(I) = 0
            ADDR(I) = 0
            OFFST(I) = 0
            PNAME(I) = ' '
 10         CONTINUE
C                                       get memory
      ELSE IF (OPCODE(:3).EQ.'GET') THEN
C                                       find free word
         I = TABSIZ + 1
         DO 20 J = 1,TABSIZ
            IF (NWDS(J).LE.0) I = MIN (I, J)
 20         CONTINUE
         IF (I.GT.TABSIZ) THEN
            IERR = 2
            MSGTXT = 'MEMORY TABLE FULL AT ' // NAME
            DO 25 J = 1,TABSIZ
               CALL MSGWRT (8)
               WRITE (MSGTXT,1020) J, PNAME(J), NWDS(J), OFFST(J)
 25            CONTINUE
            GO TO 990
         ELSE
            N = 4 * KWORDS
            J = 0
C                                       ZMEMR2 takes kilobytes now
            CALL ZMEMR2 (J, N, NBASE, ADDR(I), OFFSET, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ZMEMRY: MEMORY ALLOCATION FAILS AT ' // NAME
               IERR = 3
               GO TO 990
            ELSE
               NWDS(I) = KWORDS
               OFFST(I) = OFFSET
               PNAME(I) = NAME
               END IF
            END IF
C                                       free one
      ELSE IF (OPCODE(:4).EQ.'FREE') THEN
C                                       find it
         I = TABSIZ + 1
         DO 30 J = 1,TABSIZ
            IF ((PNAME(J).EQ.NAME) .AND. (OFFST(J).EQ.OFFSET))
     *         I = MIN (I, J)
 30         CONTINUE
         IF (I.GT.TABSIZ) THEN
            IERR = 1
            MSGTXT = 'ZMEMRY: ENTRY NOT FOUND AT ' // NAME
            GO TO 990
         ELSE
            N = ((NWDS(I)-1) / 256) + 1
            N = 1024 * N
            J = 1
            CALL ZMEMR2 (J, N, NBASE, ADDR(I), OFFSET, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'MEMORY FREE FAILS AT ' // NAME
               IERR = 4
               GO TO 990
            ELSE
               NWDS(I) = 0
               ADDR(I) = 0
               OFFST(I) = 0
               PNAME(I) = ' '
               END IF
            END IF
C                                       free one
      ELSE IF ((OPCODE(:4).EQ.'FRAL') .OR. (OPCODE(:4).EQ.'QUIT')) THEN
C                                       find it
         M = 0
 40      I = TABSIZ + 1
C                                       FRAL uses subroutine name
            IF (OPCODE(:4).EQ.'FRAL') THEN
               DO 45 J = 1,TABSIZ
                  IF (PNAME(J).EQ.NAME) I = J
 45               CONTINUE
C                                       QUIT kills all starting at high
            ELSE
               DO 46 J = 1,TABSIZ
                  IF (NWDS(J).GT.0) I = J
 46               CONTINUE
               END IF
            IF (I.GT.TABSIZ) THEN
               IF ((M.GT.0) .OR. (OPCODE(:4).EQ.'QUIT')) GO TO 999
               IERR = 1
               MSGTXT = 'ENTRY NOT FOUND AT ' // NAME
               GO TO 990
            ELSE
               N = ((NWDS(I)-1) / 256) + 1
               N = 1024 * N
               J = 1
               CALL ZMEMR2 (J, N, NBASE, ADDR(I), OFFSET, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'MEMORY FREE FAILS AT ' // NAME
                  IERR = 4
                  GO TO 990
               ELSE
                  M = M + 1
                  NWDS(I) = 0
                  ADDR(I) = 0
                  OFFST(I) = 0
                  PNAME(I) = ' '
                  END IF
               END IF
            GO TO 40
C                                       bad opcode
      ELSE
         IERR = 5
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ZMEMRY: LIST(',I3,') NAME, WORDS, OFFSET ',A,I8,I14)
      END


