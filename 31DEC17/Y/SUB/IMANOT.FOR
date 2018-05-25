      SUBROUTINE IMANOT (OP, IG, X, Y, IANGL, CENTER, STRING, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C! draws a character string with black background to graphics
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000
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
C   IMANOT is used to annotate an image by writing the string into the
C   lettering plane (usually graphics plane 2) and, if possible writing
C   a block of ones NEDGE pixels wider than the string into graphics
C   plane 4 to force a black background:     NEDGE = 2 * MAXXTV / 512
C   Inputs:
C      OP      C*4     'ONNN' enables the 2 graphics planes
C                      'OFFF' disables the 2 planes
C                      'INIT' zeros and enables the 2 planes
C                      'WRIT' writes strings to the planes
C      IG      I       Preferred graphics channel (0 => 2)
C      X       I       X position of string
C      Y       I       Y position of string
C      IANGL   I       0 - horizontal, 3 - vertical (DOWN)
C      CENTER  I       0 - XY are lower left first character
C                      1 - XY are center of string
C                      2 - XY are top right of last character
C      STRING  C*(*)   character string: length from LEN (STRING)
C   Output:
C      SCRTCH  I(*)    scratch buffer (> 1280)
C      IERR    I       error code of ZM70XF : 0 - ok
C                                                  2 - input error
C-----------------------------------------------------------------------
      CHARACTER OP*4, STRING*(*)
      INTEGER   IG, X, Y, IANGL, CENTER, SCRTCH(*), IERR
C
      CHARACTER LOP*4, OPCODE(4)*4
      INTEGER   ICC, ICB, NOP, X0, Y0, COUNT, NEDGE, IOP, X1, Y1
      LOGICAL   DOBACK, WASYNC
      INCLUDE 'INCS:DTVC.INC'
      DATA NOP, OPCODE /4,'INIT','ONNN','OFFF','WRIT'/
C-----------------------------------------------------------------------
      NEDGE = (2 * MAXXTV(1)) / 512
      IF (NEDGE.LT.2) NEDGE = 2
      IF (NGRAPH.LE.0) GO TO 999
      COUNT = LEN (STRING)
      LOP = OP
      ICC = IG
      IF ((ICC.LE.0) .OR. (ICC.EQ.NGRAPH)) ICC = 2
      ICC = MAX (1, MIN (ICC, NGRAPH-1))
      ICC = ICC + NGRAY
      ICB = NGRAPH + NGRAY
      DOBACK = NGRAPH.GE.4
C                                        check input
      IERR = 2
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 15
 10      CONTINUE
      GO TO 999
 15   IF (IOP.EQ.4) THEN
         IF (COUNT.LT.1) GO TO 999
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 999
         END IF
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', Y1)
C                                        zero graphics
 20   IF (IOP.GT.1) GO TO 30
         CALL YZERO (ICC, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOBACK) CALL YZERO (ICB, IERR)
         IF (IERR.NE.0) GO TO 990
         LOP = OPCODE(2)
C                                        enable/disable graphics
 30   IF (IOP.GT.3) GO TO 40
         CALL YSLECT (LOP, ICC, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOBACK) THEN
            CALL YSLECT (LOP, ICB, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                        WRITe char string
 40   IF (IOP.NE.4) GO TO 990
         CALL IMCHAR (ICC, X, Y, IANGL, CENTER, STRING, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       background: get start & size
         IF (.NOT.DOBACK) GO TO 990
C                                       find start position: lower
C                                       left of first char
         X0 = X
         Y0 = Y
C                                       centering
         IF (CENTER.EQ.1) THEN
            IF (IANGL.EQ.3) THEN
               X0 = X - CSIZTV(1)/2
               Y0 = Y + (COUNT*CSIZTV(2)+1)/2 - CSIZTV(2)
            ELSE IF (IANGL.EQ.2) THEN
               X0 = X + (COUNT*CSIZTV(1)+1)/2 - CSIZTV(1)
               Y0 = Y - CSIZTV(2)/2
            ELSE IF (IANGL.EQ.1) THEN
               X0 = X - CSIZTV(1)/2
               Y0 = Y - (COUNT*CSIZTV(2)+1)/2
            ELSE
               X0 = X - (COUNT*CSIZTV(1))/2
               Y0 = Y - CSIZTV(2)/2
               END IF
C                                        upper right of last
         ELSE IF (CENTER.EQ.2) THEN
            IF (IANGL.EQ.3) THEN
               X0 = X - CSIZTV(1) + 1
               Y0 = Y + CSIZTV(2)*(COUNT-2) + 1
            ELSE IF (IANGL.EQ.2) THEN
               Y0 = Y - CSIZTV(2) + 1
               X0 = X - CSIZTV(1) + 1
            ELSE IF (IANGL.EQ.1) THEN
               X0 = X - CSIZTV(1) + 1
               Y0 = Y - CSIZTV(2)*COUNT + 1
            ELSE
               Y0 = Y - CSIZTV(2) + 1
               X0 = X - COUNT*CSIZTV(1) + 1
               END IF
            END IF
         IF (IANGL.EQ.3) THEN
            X1 = X0 + CSIZTV(1) + NEDGE
            X0 = X0 - NEDGE
            Y1 = Y0 + CSIZTV(2) + NEDGE
            Y0 = Y0 - (COUNT-1)*CSIZTV(2) - NEDGE
         ELSE IF (IANGL.EQ.2) THEN
            X1 = X0 + CSIZTV(1) + NEDGE
            X0 = X0 - (COUNT-1)*CSIZTV(1) - NEDGE
            Y1 = Y0 + CSIZTV(2) + NEDGE
            Y0 = Y0 - NEDGE
         ELSE IF (IANGL.EQ.1) THEN
            X1 = X0 + CSIZTV(1) + NEDGE
            Y1 = Y0 + COUNT*CSIZTV(2) + NEDGE
            X0 = X0 - NEDGE
            Y0 = Y0 - NEDGE
         ELSE
            X1 = X0 + COUNT*CSIZTV(1) + NEDGE
            Y1 = Y0 + CSIZTV(2) + NEDGE
            X0 = X0 - NEDGE
            Y0 = Y0 - NEDGE
            END IF
         X0 = MAX (1, X0)
         Y0 = MAX (1, Y0)
         X1 = MIN (X1, MAXXTV(1))
         Y1 = MIN (Y1, MAXXTV(2))
C                                        Have YFILL do it
         CALL YFILL (ICB, X0, Y0, X1, Y1, 1, SCRTCH, IERR)
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', Y1)
C
 999  RETURN
      END
