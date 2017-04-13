      SUBROUTINE ZPARAL (NPROC)
C-----------------------------------------------------------------------
C! Set and report on number of processors to use for parallel tasks
C# Z General
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   If NPROC is positive then set the number of threads of execution to
C   be NPROC for subsequent tasks.  Set NPROC to the actual number of
C   threads of execution regardless of its initial value.
C
C   Input/output:
C      NPROC     I       On entry: the number of threads/processors
C                                  requested
C                        On exit: the actual number of threads that
C                                 will be used
C
C   IRIX version: sets/reads the MP_SET_NUMTHREADS environment variable
C-----------------------------------------------------------------------
      INTEGER   NPROC
C
C   Local variables
C
C   LNAME       Logical name used to set parallelism (constant)
C   NUMBUF      Character buffer for translating numbers
C   TRBUF       Translation buffer for logical names
C   XLATED      Number of characters in translated names
C   START       Pointer to first non-zero digit in NUMBUF
C   IRET        Return code from ZCRLOG and ZTRLOG
C
      CHARACTER LNAME*17, NUMBUF*20, TRBUF*20
      INTEGER   XLATED, START, IRET
C                                       Note that a 20-character buffer
C                                       holds the decimal version of a
C                                       16-bit binary integer.
      PARAMETER (LNAME = 'MP_SET_NUMTHREADS')
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NPROC .GT. 0) THEN
         WRITE (NUMBUF, 1000) NPROC
         START = 1
C
C        Simulated while loop
C        Invariant: NUMBUF(1:START - 1) is all zeros
C        Bound: LEN (NUMBUF) - START
C        Note that the last character is guaranteed not to be zero
C        by the restriction that NPROC > 0
C
   10    IF (NUMBUF(START:START) .EQ. '0') THEN
            START = START + 1
            GO TO 10
         END IF
         CALL ZCRLOG (LEN (LNAME), LNAME, LEN (NUMBUF) - START + 1,
     *      NUMBUF(START:LEN (NUMBUF)), IRET)
         IF (IRET .NE. 0) THEN
C
C           Can not update logical so issue an error message and try
C           and read back the actual value.
C
            MSGTXT = 'ZPARAL: CAN NOT SET ''' // LNAME // ''''
            CALL MSGWRT (6)
            IRET = 0
            CALL ZTRLOG (LEN (LNAME), LNAME, LEN (TRBUF), TRBUF, XLATED,
     *         IRET)
            IF (IRET .EQ. 0) THEN
C
C              Right-justify the translated value in NUMBUF:
C
               NUMBUF = ' '
               NUMBUF(20 - XLATED + 1:20) = TRBUF(1:XLATED)
               READ (NUMBUF, 1001, IOSTAT = IRET) NPROC
               IF (IRET .EQ. 0) THEN
C
C                 Filter out obviously silly values:
C
                  NPROC = MAX (NPROC, 1)
               ELSE
C
C                 The value couldn't be translated to an integer so
C                 assume that only one processor will be used:
C
                  MSGTXT = 'ZPARAL: ''' // LNAME //
     *               ''' HAS A NON-NUMERIC VALUE'
                  CALL MSGWRT (6)
                  MSGTXT = 'ZPARAL: ASSUMING ONLY ONE PROCESSOR ' //
     *               'WILL BE USED'
                  CALL MSGWRT (6)
                  NPROC = 1
               END IF
            ELSE
C
C              Can not read the logical variable. The most likely
C              reason is that it is not set so assume that only
C              one processor will be used and don't bother the
C              user:
C
               NPROC = 1
            END IF
         END IF
      ELSE
         CALL ZTRLOG (LEN (LNAME), LNAME, LEN (TRBUF), TRBUF, XLATED,
     *      IRET)
         IF (IRET .EQ. 0) THEN
C
C           Right-justify the translated value in NUMBUF:
C
            NUMBUF = ' '
            NUMBUF(20 - XLATED + 1:20) = TRBUF(1:XLATED)
            READ (NUMBUF, 1001, IOSTAT = IRET) NPROC
            IF (IRET .EQ. 0) THEN
C
C              Filter out obviously silly values:
C
               NPROC = MAX (NPROC, 1)
            ELSE
C
C              The value couldn't be translated to an integer so
C              assume that only one processor will be used:
C
               MSGTXT = 'ZPARAL: ''' // LNAME //
     *            ''' HAS A NON-NUMERIC VALUE'
               CALL MSGWRT (6)
               MSGTXT = 'ZPARAL: ASSUMING ONLY ONE PROCESSOR ' //
     *            'WILL BE USED'
               CALL MSGWRT (6)
               NPROC = 1
            END IF
         ELSE
C
C           Can not read the logical variable. The most likely
C           reason is that it is not set so assume that only
C           one processor will be used and don't bother the
C           user:
C
            NPROC = 1
         END IF
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I20.20)
 1001 FORMAT (I20)
      END
