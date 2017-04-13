      SUBROUTINE LWIO (OP, LWLUN, N, BUF, IERR)
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
C   LWIO moves characters to the buffer and if needed causes an I/O
C   to occur to the LaserWriter print file.
C   Inputs: LWLUN    I*2     LUN of file
C           N        I*2     Number of new characters
C           BUF      C*256   Character string
C   Output: IERR     I*2     Error code: 0 -> ok, else I/O error
C-----------------------------------------------------------------------
      CHARACTER OP*4, BUF*(*)
      INTEGER LWLUN, N, IERR, N1, I
      DATA N1/1/
      INCLUDE 'LWPLA.INC'
C-----------------------------------------------------------------------
      IF (OP.NE.'OPEN') GO TO 100
         IERR = 0
         CALL ZLWIO ('OPEN', LWLUN, OUTFIL, N1, LWBUFF, IERR)
	 RETURN
C
 100  IF (OP.NE.'WRIT') GO TO 200
         IERR = 0
         IF (N.GT.0.AND.N.LE.LWSIZE) GO TO 10
            IERR = 10
            RETURN
C                                       Do an I/O , if there's not room
  10     IF (LWPOS+N.LE.LWSIZE) GO TO 30
            CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF, IERR)
            IF (IERR.NE.0) RETURN
            LWPOS = 0
C                                       Just move characters
  30     DO I = 1,N
             LWBUFF(LWPOS+I:LWPOS+I) = BUF(I:I)
	 ENDDO
         LWPOS = LWPOS + N
	 RETURN
C
 200  IF (OP.NE.'FLUS') GO TO 300
         IERR = 0
         IF (LWPOS.EQ.0) RETURN
         CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF, IERR)
         IF (IERR.NE.0) RETURN
         LWPOS = 0
RETURN
C
 300  IF (OP.NE.'CLOS') GO TO 400
         IERR = 0
         CALL ZLWIO ('CLOS', LWLUN, OUTFIL, N1, LWBUFF, IERR)
         RETURN
C
 400  IERR = 1
C
      RETURN
      END
