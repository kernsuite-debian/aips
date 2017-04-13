      SUBROUTINE TVFIND (MAXPL, TYPE, IPL, UNIQUE, CATBLK, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C! determines which of the visibile images on the TV the user desires
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008-2009
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
C   TVFIND determines which of the visible TV images the user wishes to
C   select.  If there is more than one visible image, it requires the
C   user to point at it with the cursor.  The TV must already be open.
C   Inputs:
C      MAXPL   I        Highest plane number allowed (i.e. do graphics
C                       planes count?)
C      TYPE    C*2      2-char image type to restrict search
C   Output:
C      IPL     I        Plane number found
C      UNIQUE  L        T => only one image visible now
C                       (all types except zeroed ones ('ZZ'))
C      CATBLK  I(256)   Image catalog block found
C      SCRTCH  I(256)   Scratch buffer
C      IERR    I        Error code: 0 => ok
C                          1 => no image
C                          2 => IO error in image catalog
C                          3 => TV error
C                          4 => in ROAM mode
C-----------------------------------------------------------------------
      INTEGER   MAXPL, IPL, CATBLK(256), SCRTCH(256), IERR
      CHARACTER TYPE*2
      LOGICAL   UNIQUE
C
      CHARACTER CHTEMP*2
      INTEGER   IER, I, J, KPL, IROUND, IBUT, ITEMP, ZAND
      REAL      RPOS(2)
      LOGICAL   ANY, MISSED
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL YFIND (MAXPL, TYPE, IPL, UNIQUE, CATBLK, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 999
      IF (IERR.NE.10) THEN
         WRITE (MSGTXT,1000) IERR
         IF (IERR.EQ.1) WRITE (MSGTXT,1001) TYPE
         IF (IERR.EQ.2) WRITE (MSGTXT,1002)
         GO TO 990
         END IF
C                                       ROAM mode
      ITEMP = 0
      DO 10 I = 1,3
         DO 10 J = I+1,4
            IF (TVLIMG(I).NE.TVLIMG(J)) ITEMP = ITEMP + 1
 10         CONTINUE
      IF (ITEMP.GT.0) THEN
         IF ((TVSCRX(1).EQ.0) .AND. (TVSCRY(1).EQ.0)) THEN
            MSGTXT = 'IN STRANGE QUADRANT STATE'
         ELSE IF ((ITEMP.EQ.6) .OR. (ITEMP.EQ.4)) THEN
            MSGTXT = 'IN ROAM STATE, USE ROAMOFF VERB'
         ELSE
            MSGTXT = 'MAYBE IN ROAM STATE, TRY ROAMOFF VERB'
            END IF
         IERR = 4
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       set parms
      ANY = TYPE.EQ.'  '
      IERR = 0
C                                       Not unique: ask user
      IF (TYPE.EQ.'MA') THEN
         WRITE (MSGTXT,1010) 'image'
      ELSE IF (TYPE.EQ.'WE') THEN
         WRITE (MSGTXT,1010) 'wedge'
      ELSE
         WRITE (MSGTXT,1010) TYPE
         END IF
      CALL MSGWRT (1)
      CALL TVWHER (KPL, RPOS, IBUT, IER)
      IF (IER.EQ.0) GO TO 20
         IERR = 3
         WRITE (MSGTXT,1011) IER
         GO TO 990
C                                       Find first image w this point
 20   I = IROUND (RPOS(1))
      J = IROUND (RPOS(2))
      MISSED = .FALSE.
      DO 40 IPL = 1,MAXPL
         ITEMP = 2 ** (IPL-1)
         IF (ZAND(TVLIMG(KPL), ITEMP).EQ.0) GO TO 40
            CALL YCREAD (IPL, I, J, CATBLK, IER)
            IF (IER.EQ.0) GO TO 30
            IF (IER.EQ.1) GO TO 40
               IERR = 2
               WRITE (MSGTXT,1020) IER
               GO TO 990
 30         IF (ANY) GO TO 999
               CALL H2CHR (2, KHPTYO, CATBLK(KHPTY), CHTEMP)
               IF (TYPE.EQ.CHTEMP) GO TO 999
                  MISSED = .TRUE.
 40      CONTINUE
C                                       Not in any image
      IERR = 1
      WRITE (MSGTXT,1040) I, J
      IF (MISSED) WRITE (MSGTXT,1041) I, J, TYPE
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVFIND: YFIND RETURNS ERROR',I7)
 1001 FORMAT ('TVFIND: NO ',A2,' IMAGES ARE ON ACCORDING TO CATALOG')
 1002 FORMAT ('TVFIND: YFIND HAD IMAGE CATALOG IO ERROR')
 1010 FORMAT ('Move cursor to desired ',A,' please.  Then press any',
     *   ' button')
 1011 FORMAT ('TVFIND: TV ERROR READING CURSOR',I7)
 1020 FORMAT ('TVFIND: IMAGE CATALOG IO ERROR',I7)
 1040 FORMAT (2I7,' NOT IN ANY IMAGE')
 1041 FORMAT (2I7,' NOT IN AN IMAGE OF TYPE ',A2)
      END
