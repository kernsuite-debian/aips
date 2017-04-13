      SUBROUTINE YDTVC (OP, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! Read/write TV parameter common
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 2008
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
C   XAS (SSS) Version - parameters are in XAS, locking done by daemon
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SCRTCH(*), IERR
C
      INTEGER   I, OPCODE, DAT(4), NWSD, TVT(256)
      CHARACTER PARTYP(21)*6
      REAL      TEMP
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TVT(1), NGRAY)
      DATA PARTYP /'NGrey','NGraph','NImage','MaxinX','MaxinY','MxGrey',
     *   'LUTout','OFMin','OFMout','ScrinX','ScrinY','MxZoom','Split?',
     *   'ALUnum','WriteX','WriteY','ISUnum','TV3col','XAScat','ChrinX',
     *   'ChrinY'/
C-----------------------------------------------------------------------
C                                       permission to continue?
      IF (OP.EQ.'OPEN') THEN
         CALL ZSSSLK ('LOCK', IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.4)) THEN
            WRITE (MSGTXT,1000) 'LOCK', IERR
            CALL MSGWRT (6)
            END IF
C                                       give it up
      ELSE IF (OP.EQ.'CLOS') THEN
         CALL ZSSSLK ('UNLK', IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'UNLOCK', IERR
            CALL MSGWRT (6)
            IERR = 0
            END IF
C                                       request the status
      ELSE IF ((OP.EQ.'READ') .OR. (OP.EQ.'GET ')) THEN
C                                       was 13
         OPCODE = 10
         CALL FILL (4, 0, DAT)
         CALL ZSSSXF (OPCODE, DAT, 0, SCRTCH, NWSD, IERR)
         IF ((NWSD.NE.1024) .AND. (IERR.EQ.0)) IERR = 11
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            IF (IERR.EQ.11) WRITE (MSGTXT,1011) NWSD
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       any changes in 1-21 ?
         IF (OP.EQ.'READ') THEN
            CALL ZI32IL (256, 1, SCRTCH, SCRTCH)
            DO 10 I = 1,21
               IF ((SCRTCH(I).LT.0) .AND. (I.NE.12)) GO TO 10
               IF ((SCRTCH(I).EQ.0) .AND. (I.LE.14)) GO TO 10
               IF (SCRTCH(I).NE.TVT(I)) THEN
                  WRITE (MSGTXT,1015) PARTYP(I), TVT(I), SCRTCH(I)
                  CALL MSGWRT (6)
                  TVT(I) = SCRTCH(I)
                  END IF
 10            CONTINUE
            CALL COPY (235, SCRTCH(22), TVT(22))
         ELSE
            CALL ZI32IL (256, 1, SCRTCH, TVT)
            END IF
         IF (TVT(22).GT.0) THEN
            TEMP = TVT(22) / 1000.0
            TVGAMA = TEMP
            IF (TVGAMA.LT.0.1) TVGAMA = 2.2
         ELSE
            TVGAMA = 2.2
            END IF
C                                       send the status
      ELSE IF (OP.EQ.'WRIT') THEN
C                                       was 17
         OPCODE = 36
         CALL FILL (4, 0, DAT)
         CALL COPY (256, TVT, SCRTCH)
         SCRTCH(22) = TVGAMA * 1000.0 + 0.5
         CALL ZILI32 (256,  SCRTCH, 1, SCRTCH)
         CALL ZSSSXF (OPCODE, DAT, 1024, SCRTCH, NWSD, IERR)
C                                       bad op
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ON ',A,' ERROR',I5,' FROM ZSSSLK')
 1010 FORMAT ('ERROR',I5,' REQUESTING PARAMETERS FROM SCREEN SERVER')
 1011 FORMAT ('ONLY',I4,' PARAMETERS RETURNED FROM SCREEN SERVER!')
 1015 FORMAT ('TV parameter ',A,' has changed from',I5,' to',I5)
      END
