      SUBROUTINE YDTVC (OP, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! Read/write TV parameter common
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997
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
C   YDTVC gets or saves the TV device parameter common
C   Input:
C      OP      C*4      'READ' parameters, comment if changed
C                       'GET ' = 'READ' with no comments
C                       'WRIT' parameters to save area (disk or TV)
C                       'OPEN' open access to TV (disk lock or ..)
C                       'CLOS' clsoe access to TV
C   Output:
C      SCRTCH  I(256)   scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C                                                4 => in use on open
C   In/out:
C      DTVC.INC common
C   Generic (disk-based) version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SCRTCH(*), IERR
C
      INTEGER   TVT(256), I, MSGSAV
      CHARACTER PARTYP(21)*6, TVNAM*48
      LOGICAL   T, F
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TVT(1), NGRAY)
      DATA PARTYP /'NGrey','NGraph','NImage','MaxinX','MaxinY','MxGrey',
     *   'LUTout','OFMin','OFMout','ScrinX','ScrinY','MxZoom','Split?',
     *   'ALUnum','WriteX','WriteY','ISUnum','TV3col','XAScat','ChrinX',
     *   'ChrinY'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open disk file:
      IF (OP.EQ.'OPEN') THEN
C                                       impatient, try 4 times
         MSGSAV = MSGSUP
         CALL ZPHFIL ('ID', 1, 0, NTVDEV, TVNAM, IERR)
         DO 10 I = 1,4
            MSGSUP = 32000
            CALL ZOPEN (TVLUN2, TVIND2, 1, TVNAM, F, T, F, IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.0) GO TO 999
 10         CONTINUE
         IF (IERR.NE.1) TVIND2 = 0
C                                       close disk file
      ELSE IF (OP.EQ.'CLOS') THEN
         CALL ZCLOSE (TVLUN2, TVIND2, IERR)
C                                       request the status
      ELSE
         CALL ZFIO ('READ', TVLUN2, TVIND2, 1, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ FROM ID FILE'
            CALL MSGWRT (7)
C                                       get all
         ELSE IF (OP.EQ.'GET ') THEN
            CALL COPY (256, SCRTCH, TVT)
            IF (TVGAMA.LT.0.1) TVGAMA = 2.2
C                                       any changes in 1-21 ?
         ELSE IF (OP.EQ.'READ') THEN
            DO 20 I = 1,21
               IF ((SCRTCH(I).LT.0) .AND. (I.NE.12)) GO TO 20
               IF ((SCRTCH(I).EQ.0) .AND. (I.LE.14)) GO TO 20
               IF (SCRTCH(I).NE.TVT(I)) THEN
                  WRITE (MSGTXT,1015) PARTYP(I), TVT(I), SCRTCH(I)
                  CALL MSGWRT (6)
                  TVT(I) = SCRTCH(I)
                  END IF
 20            CONTINUE
            CALL COPY (235, SCRTCH(22), TVT(22))
            IF (TVGAMA.LT.0.1) TVGAMA = 2.2
C                                       send the status
         ELSE IF (OP.EQ.'WRIT') THEN
            CALL COPY (235, TVT(22), SCRTCH(22))
            CALL ZFIO ('WRIT', TVLUN2, TVIND2, 1, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, 'WRITE TO ID FILE'
               CALL MSGWRT (7)
               END IF
C                                       bad op
         ELSE
            IERR = 2
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' on ', A)
 1015 FORMAT ('TV parameter ',A,' has changed from',I5,' to',I5)
      END
