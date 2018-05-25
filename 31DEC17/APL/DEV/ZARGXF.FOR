      SUBROUTINE ZARGXF (OP, NBYTES, HEADER, BUFFER, IERR)
C-----------------------------------------------------------------------
C! translates IIS Model 70 commands into calls to ZARGS for ARGS TV
C# Z TV-IO
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
C   ZARGXF causes an IO operation to the TV television device
C   Inputs:
C      OP       C*4    'READ' from TV / 'WRIT' to TV
C      NBYTES   I      number bytes of data in BUFFER
C      HEADER   I(8)   TV control header
C   In/Out:
C      BUFFER   I(*)   data buffer
C   Output:
C      IERR     I      error code: 0 => ok
C                         1 => file not open
C                         2 => input error
C                         3 => IO error
C                         4 => end of file
C                         5 => beginning of medium
C                         6 => end of medium
C                         7 => timeout/inoperative
C   Generic version - calls the entries of ZARGS (which only has a VMS
C   version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   NBYTES, HEADER(8), BUFFER(*), IERR
C
      INTEGER   I, J, K, L, M, N, BITS(16), IOP, BUTTON, PLANES, ITEMP,
     *   COLOR(3), OVERLA(512), X, Y, LVT(2,256), OFM(3,256),
     *   LUT(3,256), GRAPH(9)
      REAL      A
      LOGICAL   COMAND, ON
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA A /1./
      DATA COLOR /3*0/
C-----------------------------------------------------------------------
C                                        check errors
      IERR = 2
      IF (NBYTES.LT.0) GO TO 999
      IF (MOD(NBYTES, 2).NE.0) GO TO 999
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 999
      IERR = 1
      IF (FTAB(TVIND).NE.TVLUN) GO TO 999
      IERR = 0
C
      CALL ZGTBIT (16, HEADER(3), BITS)
      CALL ZPTBIT (5, IOP, BITS(1))
      IF ((IOP.LT.1) .OR. (IOP.GT.13)) GO TO 900
      COMAND = BITS(16).EQ.1
C
      GO TO (10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130) IOP
C-----------------------------------------------------------------------
C                                        SUBUNIT 1
C                                        Refresh Memory
C-----------------------------------------------------------------------
 10   CONTINUE
         IF (OP.NE.'WRIT')  GO TO 999
C                                       Get origin & size
         CALL ZGTBIT (16, HEADER(5), BITS)
         CALL ZPTBIT (9, K, BITS)
         IF (BITS(15).EQ.1) N = 1
         IF (BITS(16).EQ.1) N = -HEADER(2)
         CALL ZGTBIT (16, HEADER(6), BITS)
         CALL ZPTBIT (9, L, BITS)
         IF (BITS(15).EQ.1) M = -HEADER(2)
         IF (BITS(16).EQ.1) M = 1
         L = 512 - L
C                                       Image plane
         IF (HEADER(7).GE.0) THEN
            CALL ZARG13 (K, L, BUFFER, N, M)
C                                       Graphics planes
         ELSE
            PLANES = HEADER(8)
            IF (PLANES.GE.8) GO TO 999
            CALL ZARG14 (K, L, OVERLA, N, M)
            ITEMP = N * M
            DO 15 I = 1,ITEMP
               CALL ZGTBIT (8, OVERLA(I), BITS(1))
               IF (BUFFER(I).EQ.-1) CALL ZGTBIT (3, PLANES, BITS(9))
               IF (BUFFER(I).EQ.0) CALL ZGTBIT (3, 0, BITS(9))
               CALL ZPTBIT (11, OVERLA(I), BITS)
 15            CONTINUE
            CALL ZARG13 (K, L, OVERLA, N, M)
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 2
C                                        Look-Up Tables (& Split Screen)
C-----------------------------------------------------------------------
 20   CONTINUE
         IF (COMAND) THEN
            WRITE (MSGTXT,1020)
            GO TO 990
            END IF
         IF (OP.NE.'WRIT') GO TO 999
         IF (HEADER(8).GT.1) THEN
            WRITE (MSGTXT,1022)
            GO TO 990
            END IF
         CALL ZGTBIT (3, HEADER(7), BITS)
         DO 25 I = 1,256
            IF (BITS(1).EQ.1) LUT(1,I) = BUFFER(I)
            IF (BITS(2).EQ.1) LUT(2,I) = BUFFER(I)
            IF (BITS(3).EQ.1) LUT(3,I) = BUFFER(I)
 25         CONTINUE
         GO TO 35
C-----------------------------------------------------------------------
C                                        SUBUNIT 3
C                                        Output Function Memory
C-----------------------------------------------------------------------
 30   CONTINUE
         IF (COMAND) GO TO 999
         IF (OP.NE.'WRIT') GO TO 999
         IF (HEADER(8).GT.1) THEN
            WRITE (MSGTXT,1022)
            GO TO 990
            END IF
         CALL ZGTBIT (3, HEADER(7), BITS)
         DO 33 I = 1,256
            IF (BITS(1).EQ.1) OFM(1,I) = BUFFER(I) / 4
            IF (BITS(2).EQ.1) OFM(2,I) = BUFFER(I) / 4
            IF (BITS(3).EQ.1) OFM(3,I) = BUFFER(I) / 4
 33         CONTINUE
 35      IF (BITS(1).EQ.1) COLOR(1) = 1
         IF (BITS(2).EQ.1) COLOR(2) = 1
         IF (BITS(3).EQ.1) COLOR(3) = 1
         IF ((COLOR(1).EQ.1) .AND. (COLOR(2).EQ.1) .AND.
     *      (COLOR(3).EQ.1)) THEN
            COLOR(1) = 0
            COLOR(2) = 0
            COLOR(3) = 0
            DO 36 I = 1,256
               CALL ZGTBIT (8, OFM(1,LUT(1,I)+1), BITS(1))
               CALL ZGTBIT (8, OFM(2,LUT(2,I)+1), BITS(9))
               CALL ZPTBIT (16, LVT(1,I), BITS)
 36            CONTINUE
            CALL FILL (8, 0, BITS(9))
            DO 37 I = 1,256
               CALL ZGTBIT (8, OFM(3,LUT(3,I)+1), BITS(1))
               CALL ZGTBIT (3, GRAPH(MOD(I-1,8)+1), BITS(13))
               CALL ZPTBIT (16, LVT(2,I), BITS)
 37            CONTINUE
            CALL ZARG05 (LVT)
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 4
C                                        Input Function Memory
C-----------------------------------------------------------------------
 40   CONTINUE
         WRITE (MSGTXT,1040)
         GO TO 990
C-----------------------------------------------------------------------
C                                        SUBUNIT 5
C                                        Feedback
C-----------------------------------------------------------------------
 50   CONTINUE
         IF (OP.NE.'WRIT') GO TO 999
         IF (BUFFER(1).GE.0) THEN
            WRITE (MSGTXT,1050)
            GO TO 990
            END IF
C                                       Clear
C                                       Image plane - CLEAR
         IF (HEADER(7).GE.0) THEN
            CALL ZARG01
C                                       Graphics planes - ERASE
         ELSE
            PLANES = HEADER(8)
            CALL ZARG02 (PLANES)
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 6
C                                        Scroll
C-----------------------------------------------------------------------
 60   CONTINUE
         IF (OP.NE.'WRIT') GO TO 999
         I = 2 * (MOD (767 - BUFFER(1), 512) - 255)
         J = 512 - 2 * (MOD (767 - BUFFER(2), 512) - 255)
         CALL ZARG16 (I, J)
         ON = .FALSE.
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 7
C                                        Videometer
C-----------------------------------------------------------------------
 70   CONTINUE
         WRITE (MSGTXT,1070)
         GO TO 990
C-----------------------------------------------------------------------
C                                        SUBUNIT 8
C                                        Sum Processor
C-----------------------------------------------------------------------
 80   CONTINUE
         WRITE (MSGTXT,1080)
         GO TO 990
C-----------------------------------------------------------------------
C                                        SUBUNIT 9
C                                        Graphics Video
C-----------------------------------------------------------------------
 90   CONTINUE
         IF (COMAND) GO TO 999
         IF (OP.NE.'READ') THEN
            DO 92 I = 1,9
               J = I
               IF (I.EQ.9) J = 129
               CALL ZGTBIT (16, BUFFER(J), BITS)
               BITS(1) = BITS(15)
               BITS(2) = BITS(10)
               BITS(3) = BITS(5)
               CALL ZPTBIT (3, GRAPH(I), BITS)
 92            CONTINUE
            CALL FILL (8, 0, BITS(9))
            DO 93 I = 1,256
               CALL ZGTBIT (8, OFM(3,LUT(3,I)+1), BITS(1))
               CALL ZGTBIT (3, GRAPH(MOD(I-1,8)+1), BITS(13))
               CALL ZPTBIT (16, LVT(2,I), BITS)
 93            CONTINUE
            CALL ZARG05 (LVT)
         ELSE
            DO 95 I = 1,9
               J = I
               IF (I.EQ.9) J = 129
               CALL ZGTBIT (3, GRAPH(I), BITS)
               BITS(5) = BITS(3)
               BITS(10) = BITS(2)
               BITS(15) = BITS(1)
               CALL ZPTBIT (16, BUFFER(J), BITS)
 95            CONTINUE
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 10
C                                        Programmable Cursor
C-----------------------------------------------------------------------
 100  CONTINUE
         IF (.NOT.COMAND) GO TO 999
C                                        Update cursor position
         IF (OP.NE.'WRIT') THEN
            CALL ZARG12 (I, J)
            CALL ZARG09 (K, L, BUTTON)
            I = MAX (0, MIN (511, I + K))
            J = MAX (0, MIN (511, J + L))
            N = 0
            IF (ON) N = GRAPH(9)
            CALL ZARG11 (I, J, N)
            CALL ZARG10 (BUTTON)
C                                       Set control word
            CALL FILL (16, 0, BITS)
            CALL ZGTBIT (7, BUTTON, BITS(6))
            BITS(6) = 0
            IF (BUTTON.GT.1) BITS(15) = 1
            CALL ZPTBIT (16, BUFFER(1), BITS)
            I = A * (I - X) + 256.5
            J = A * (J - Y) + 256.5
            BUFFER(2) = I - 32
            BUFFER(3) = 480 - J
C                                       set cursor position
         ELSE
            I = BUFFER(2) + 32
            J = 480 - BUFFER(3)
            I = X + (I - 256) / A + 0.5
            J = Y + (J - 256) / A + 0.5
C                                       Get control word
            CALL ZGTBIT (16, BUFFER(1), BITS)
            ON = BITS(1).EQ.1
            N = 0
            IF (ON) N = GRAPH(9)
            CALL ZARG11 (I, J, N)
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 11
C                                        Arithmetic/Logic Unit
C-----------------------------------------------------------------------
 110  CONTINUE
         WRITE (MSGTXT,1110)
         GO TO 990
C-----------------------------------------------------------------------
C                                        SUBUNIT 12
C                                        Zoom
C-----------------------------------------------------------------------
 120  CONTINUE
         IF (OP.EQ.'WRIT') THEN
            A = 2 ** BUFFER(1)
            I = ((A - 1) * (BUFFER(2) - 1) + 256) / A + 1.5
            J = ((A - 1) * (512 - BUFFER(3)) + 256) / A + 0.5
            N = 2 ** BUFFER(1) - 1
            CALL ZARG15 (I, J, N)
            X = I
            Y = J
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                        SUBUNIT 13
C                                        Inter-Processor Buffer
C-----------------------------------------------------------------------
 130  CONTINUE
         WRITE (MSGTXT,1130)
         GO TO 990
C-----------------------------------------------------------------------
C                                        error on header
 900  WRITE (MSGTXT,1900) IOP
C
 990  CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('TV SPLIT SCREEN IS NOT IMPLEMENTED')
 1022 FORMAT ('TV HAS ONLY ONE REFRESH MEMORY CHANNEL')
 1040 FORMAT ('TV INPUT FUNCTION MEMORY IS NOT IMPLEMENTED')
 1050 FORMAT ('TV FEEDBACK IS NOT IMPLEMENTED')
 1070 FORMAT ('TV VIDEOMETER IS NOT IMPLEMENTED')
 1080 FORMAT ('TV SUM PROCESSOR IS NOT IMPLEMENTED')
 1110 FORMAT ('TV ARITHMETIC/LOGIC UNIT IS NOT IMPLEMENTED')
 1130 FORMAT ('TV INTER-PROCESSOR BUFFER IS NOT IMPLEMENTED')
 1900 FORMAT ('ZARGXF HEADER ERROR: SUBUNIT',I4)
       END
