      SUBROUTINE GICHAR (ITY, NCHAR, IANGL, DX, DY, STR, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes a draw character string command record into a plot file or TV
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   GICHAR will write a 'draw in-plot string' command record, bright or
C   dark, to a graph file.
C   The output record description is:
C      I             opcode, 14 or 15 (dark) in this program.
C      I             number of characters (NCHAR).
C      I             angle (IANGL).
C      I             X offset in characters * 100  (DX * 100).
C      I             Y offset in characters * 100  (DY * 100).
C      I(4*NCHAR))   characters (STR).
C   The characters may go instead to the TV in TV mode.
C   Inputs:
C      ITY     I        Type: 0 characters outside main plot
C                             1 bright chars inside main plot
C                             2 dark chars inside manin plot
C      NCHAR   I        number of characters in STR
C      IANGL   I        angle to print STR. 0=horizontal, 1=vertical.
C      DX      R        x offset in characters from current position for
C                       the bottom left corner of first character printed.
C      DY      R        y offset in characters from current position for
C                       the bottom left corner of first character printed.
C      STR     C*(*)    string to be printed.
C      BUFF    I(256)   buffer to use for I/O.
C   Output:
C      IERR    I        error code:  0 => ok
C                       1 => disk problems
C                       2 => TV problems
C                       3 => string too big
C   Common:
C      GPHPOS  incremented by 5 + (NCHAR+1)/2 .
C      GPHRRN  incremented by 1 if a write to disk is needed.
C-----------------------------------------------------------------------
      INTEGER   ITY, NCHAR, IANGL, BUFF(256), IERR
      REAL      DX, DY
      CHARACTER STR*(*)
C
      INTEGER   CWORDS, NEED, OPCODE(3), NMAX, IER, JANGL, MCHAR, LX,
     *   LY, IX, IY, LCHAR, I
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA OPCODE /6,14,15/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NCHAR.LE.0) GO TO 999
      I = ITY + 1
      IF ((I.LT.1) .OR. (I.GT.3)) I = 1
C                                       plot file
      IF (.NOT.GPHDOT) THEN
C                                       Calc no. of words needed.
         CWORDS = (NCHAR+3) / 4
         NEED = 252 - CWORDS
         IF (NEED.LE.0) THEN
            IERR = 3
            NMAX = 251 * 4
            WRITE (MSGTXT,1000) NCHAR, NMAX
            GO TO 990
            END IF
C                                       See if enough room in buffer.
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) THEN
               IERR = 1
               GO TO 999
               END IF
            END IF
C                                       Put data in buffer.
         BUFF(GPHPOS+1) = OPCODE(I)
         BUFF(GPHPOS+2) = NCHAR
         BUFF(GPHPOS+3) = IANGL
         BUFF(GPHPOS+4) = DX * 100.
         BUFF(GPHPOS+5) = DY * 100.
         CALL CHR2H (NCHAR, STR, 1, BUFF(GPHPOS+6))
         GPHPOS = GPHPOS + 5 + CWORDS
C                                       TV mode
      ELSE
C                                       Channel okay?
         IF ((ITY.EQ.2) .AND. (GPHDOD)) THEN
            I = GPHTVG(8)
         ELSE
            GPHLTY = MAX (1, GPHLTY)
            I = GPHTVG(GPHLTY)
            END IF
         CALL GCINIT (I, 0, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       NOTE: plot file angle 1 => 3
         JANGL = IANGL
         IF (JANGL.EQ.1) JANGL = 3
         IX = GPHIXL + DX * CSIZTV(1)
         IY = GPHIYL + DY * CSIZTV(2)
         MCHAR = NCHAR
         LCHAR = 1
         IF (JANGL.EQ.0) THEN
            LY = IY
            LX = IX + NCHAR * CSIZTV(1) - 1
            IF ((IY.LE.0) .OR. (IY+CSIZTV(2)-1.GT.MAXXTV(2))) THEN
               GPHERC = GPHERC + 1
               GO TO 999
               END IF
            IF ((GPHPLY(2).EQ.LY) .AND. (GPHPLX(2).GT.IX) .AND.
     *         (GPHPLX(1).LE.LX)) THEN
               GPHERO = GPHERO + 1
               GO TO 999
               END IF
            IF (LX.GT.MAXXTV(1)) THEN
               GPHERC = GPHERC + 1
               NCHAR = (MAXXTV(1) + 1 - IX) / CSIZTV(1)
               IF (NCHAR.LE.0) GO TO 999
               END IF
            IF (IX.LT.1) THEN
               GPHERC = GPHERC + 1
               LCHAR = (-IX + CSIZTV(1) - 1) / CSIZTV(1) + 1
               IX = IX + (LCHAR - 1) * CSIZTV(1)
               END IF
            GPHPLY(1) = LY + CSIZTV(2) - 1
            GPHPLY(2) = LY
            GPHPLX(1) = IX
            GPHPLX(2) = IX + (NCHAR-LCHAR+1) * CSIZTV(1) - 1
         ELSE
            LX = IX
            LY = IY - (NCHAR-1) * CSIZTV(2)
            IF ((IX.LE.0) .OR. (IX+CSIZTV(1)-1.GT.MAXXTV(1))) THEN
               GPHERC = GPHERC + 1
               GO TO 999
               END IF
            IF ((GPHPLX(1).EQ.LX) .AND. (GPHPLY(2).LT.IY) .AND.
     *         (GPHPLY(1).GE.LY)) THEN
               GPHERO = GPHERO + 1
               GO TO 999
               END IF
            IF (LY.LT.1) THEN
               GPHERC = GPHERC + 1
               NCHAR = (IY - 1) / CSIZTV(2) + 1
               IF (NCHAR.LE.0) GO TO 999
               END IF
            IF (IY.GT.MAXXTV(2)) THEN
               GPHERC = GPHERC + 1
               LCHAR = (IY - MAXXTV(2) + CSIZTV(2) -1) / CSIZTV(2) + 1
               IY = IY - (LCHAR-1) * CSIZTV(2)
               END IF
            GPHPLY(1) = IY + CSIZTV(2) - 1
            GPHPLY(2) = IY - (NCHAR-LCHAR) * CSIZTV(2)
            GPHPLX(1) = IX
            GPHPLX(2) = IX + CSIZTV(1) - 1
            END IF
         IF (LCHAR.GT.NCHAR) GO TO 999
         CALL IMCHAR (I, IX, IY, JANGL, 0, STR(LCHAR:NCHAR), GPHSCR,
     *      IERR)
         IF (IERR.EQ.2) IERR = 0
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) 'IMCHAR', IERR
            IERR = 2
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GCHAR: LENGTH OF STRING:',I7,' OVER ',I4,' CHARACTERS')
 1100 FORMAT ('GCHAR: TV ROUTINE ',A,' RETURNS ERROR',I7)
      END
