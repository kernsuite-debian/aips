      SUBROUTINE OFMIO (OPER, N, NLEVS, VRTRTC, OFM, IERR)
C-----------------------------------------------------------------------
C! reads or writes the OFM from/to TV device, incl copying as needed
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   reads the 3 OFMs from the TV.  writes them to the TV spreading
C   NLEVS values per color as many times as needed to fill the table
C   Inputs:
C      OPER     C*4      READ or WRIT
C      N        I        dimension of OFM
C      NLEVS    I        number actual values known on OFM ('WRIT')
C      VRTRTC   L        do only on vertical retrace?
C   In/out:
C      OFM      R(N,3)   OFM (values 0-1) (,1) B (,2) G (,3) R
C   Output:
C      IERR     I        Error code: 2 user error, 0 okay.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   N, NLEVS, IERR
      LOGICAL   VRTRTC
      REAL      OFM(N,3)
C
      INTEGER   I, IT, J, L
      LOGICAL   WASYNC
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                        check parameters
      IERR = 2
      IT = OFMINP + 1
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) GO TO 999
      IF (N.LT.IT) GO TO 999
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', J)
C                                        copy buffer as needed
      IF (OPER.EQ.'WRIT') THEN
         IF (NLEVS.GT.IT) GO TO 990
         J = IT / NLEVS
         L = NLEVS + 1
         OFM(1,1) = 0.0
         OFM(1,2) = 0.0
         OFM(1,3) = 0.0
         IF (J.GE.2) THEN
            DO 10 I = 2,J
               CALL RCOPY (NLEVS, OFM(1,1), OFM(L,1))
               CALL RCOPY (NLEVS, OFM(1,2), OFM(L,2))
               CALL RCOPY (NLEVS, OFM(1,3), OFM(L,3))
               L = L + NLEVS
 10            CONTINUE
            END IF
         END IF
C                                        do it
      CALL YOFM (OPER, 1, VRTRTC, OFM(1,1), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YOFM (OPER, 2, VRTRTC, OFM(1,2), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YOFM (OPER, 4, VRTRTC, OFM(1,3), IERR)
      IF ((OPER.EQ.'READ') .AND. (IERR.EQ.0)) THEN
         OFM(1,1) = 0.0
         OFM(1,2) = 0.0
         OFM(1,3) = 0.0
         END IF
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', J)
C
 999  RETURN
      END
