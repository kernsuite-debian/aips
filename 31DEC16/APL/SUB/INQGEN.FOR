      SUBROUTINE INQGEN (TTY, PROMPT, LIST, IOUT, DOUT, COUT, IERR)
C-----------------------------------------------------------------------
C! inquire of user for specified list of integer, float, & char values
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Prompt user, get free form input in sequence of integer, floating,
C   or character formats - will prompt for additional lines if needed.
C   Character data may be represented as consecutive characters
C   beginning with a letter (no imbedded blanks) or may be surrounded
C   by single quotes.  Must be all on one line.
C   Inputs:
C      TTY     I(2)    (1) TTY LUN of terminal opened with ZOPEN.
C                      (2) TTY FTAB index of open terminal.
C      PROMPT  C*72    Prompt.
C      LIST    I(*)    Formats to read: -1 integer, -2 float, n > 0
C                         character*n, 0 => end of list
C   Output:
C      IOUT    I(*)    Integer results
C      DOUT    D(*)    Float results
C      COUT    C*(*)   Character results - strings concatenated
C      IERR    I       > 0 => terminal I/O error;  0 => ok;
C                      < 0 => format error in on -IERR'th output item
C-----------------------------------------------------------------------
      INTEGER   TTY(2), LIST(*), IOUT(*), IERR
      CHARACTER PROMPT*72, COUT*(*)
      DOUBLE PRECISION DOUT(*)
C
      CHARACTER KB*72
      INTEGER   I, KBP, II, ID, IC, IN,  NMATCC, IQ
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (IERR.NE.0) GO TO 999
      KBP = 1
      II = 0
      ID = 0
      IC = 0
      DO 90 I = 1,100
         IF (LIST(I).EQ.0) GO TO 999
 10      IN = NMATCC (KBP, ' ', KB)
         IF (IN.GT.72) THEN
            KB = '****  MORE VALUES PLEASE : '
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, KB, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
            IF (IERR.NE.0) GO TO 999
            KBP = 1
            GO TO 10
            END IF
         KBP = IN
C                                       numeric
         IF (LIST(I).LT.0) THEN
            IF (((KB(KBP:KBP).GE.'0') .AND. (KB(KBP:KBP).LE.'9')) .OR.
     *         (KB(KBP:KBP).EQ.'+') .OR. (KB(KBP:KBP).EQ.'-') .OR.
     *         (KB(KBP:KBP).EQ.'.')) THEN
               CALL GETNUM (KB, 72, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  IERR = -I
                  GO TO 999
               ELSE IF (LIST(I).EQ.-1) THEN
                  II = II + 1
                  IOUT(II) = X + 0.5D0
                  IF (X.LT.0.0) IOUT(II) = X - 0.5D0
               ELSE
                  ID = ID + 1
                  DOUT(ID) = X
                  END IF
C                                       bad numeric format
            ELSE
               KBP = 73
               KB = '******  BAD FORMAT - RE-ENTER CORRECTLY *****'
               CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, KB, IERR)
               IF (IERR.NE.0) GO TO 999
               GO TO 10
               END IF
C                                       character
         ELSE
C                                       quoted string
            IF (KB(KBP:KBP).EQ.'''') THEN
               KBP = KBP + 1
               IQ = INDEX (KB(KBP:), '''')
            ELSE
               IQ = INDEX (KB(KBP:), ' ')
               END IF
            IQ = IQ + KBP - 1
            IF (IQ.LT.KBP) IQ = 73
            COUT(IC+1:) = ' '
            IN = IQ - KBP
            IN = MIN (IN, LIST(I))
            COUT (IC+1:IC+IN) = KB(KBP:KBP+IN-1)
            IC = IC + LIST(I)
            KBP = IQ + 1
            END IF
 90      CONTINUE
C
 999  RETURN
      END
