      SUBROUTINE YLOCAT (NP, XP, YP, NAME, CLASS, SEQ, DISK, PTYP, IX,
     *   IY, IQ, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! return TV positions for set of image positions
C# Y0 TV-util
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
C   YLOCAT locates a set of image pixel positions on the TV for a
C   specified image using only those grey planes that are turned on.
C   Inputs:
C      NP      I         Number of pixel positions
C      XP      R(NP)     X image pixel positions
C      YP      R(NP)     Y image pixel positions
C      NAME    C*12      Packed image name (name) '    ' => any
C      CLASS   C*6       Packed image name (class) '    ' => any
C      SEQ     I         Image name (sequence #)  0 => any
C      DISK    I         Image file disk 0 => any
C      PTYP    C*2       Image type  '  ' => any
C   Output:
C      IX      I(NP)     TV x positions
C      IY      I(NP)     TV y positions
C      IQ      I(NP)     TV channels  0 => none this position
C      IERR    I         Error code: 0 -> ok
C                           2 -> input error
C                           3 -> IO error
C                          11 -> some positions bad
C                          12 -> no positions found
C   Uses common /MAPHDR/ results unpredictable except on IERR = 0
C   then = image catlg header of last position found.
C   Virtual TV version (TV by communication).
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLASS*6, PTYP*2
      INTEGER   NP, SEQ, DISK, IX(*), IY(*), IQ(*), SCRTCH(256), IERR
      REAL      XP(1), YP(1)
C
      CHARACTER XT*20
      INTEGER   ONPL, ZOR, I, IXT(10), IP
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       planes that are on
      IERR = 2
      ONPL = 0
      DO 10 I = 1,4
         ONPL = ZOR (ONPL, TVLIMG(I))
 10      CONTINUE
      IF (ONPL.EQ.0) GO TO 999
      IF (NP.LE.0) GO TO 999
C                                       pack inputs
      BUFSW = 13 + 4 * NP
      BUFSR = 13 + 299 + 7 * NP
      CALL FILL (BUFSW, 0, BUFFER)
      BUFFER(1) = NP
      XT = NAME
      XT(13:) = CLASS
      XT(19:) = PTYP
      CALL ZCLC8 (20, XT, 1, IXT)
      CALL ZI16IL (10, 1, IXT, BUFFER(2))
      BUFFER(12) = SEQ
      BUFFER(13) = DISK
      IP = 14
      DO 20 I = 1,NP
         BUFFER(IP) = XP(I)
         BUFFER(IP+1) = 1000.0 * (XP(I) - BUFFER(IP))
         BUFFER(IP+2) = YP(I)
         BUFFER(IP+3) = 1000.0 * (YP(I) - BUFFER(IP+2))
         IP = IP + 4
 20      CONTINUE
      CALL MKYBUF ('READ', 'YLOCAT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       Unpack returns
      IF ((IERR.EQ.0) .OR. (IERR.EQ.11)) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         CALL COPY (NP, BUFFER(IP), IX)
         IP = IP + NP
         CALL COPY (NP, BUFFER(IP), IY)
         IP = IP + NP
         CALL COPY (NP, BUFFER(IP), IQ)
         IP = IP + NP
         CALL HDRBUF (2, CATBLK, CATH, CATR, CATD, BUFFER(IP))
         END IF
C
 999  RETURN
      END
