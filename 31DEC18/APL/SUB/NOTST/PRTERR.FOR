      SUBROUTINE PRTERR (IERR, ILUN)
C-----------------------------------------------------------------------
C! Prints standard WaWa error message and namestring of file.
C# IO-WaWa
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
C   PRTERR will print a standard WAWA error message plus the name string
C   if the LUN can be found in FILTAB.
C   Inputs:
C     IERR       I   WAWA error number.
C     ILUN       I   the LUN of the file.
C-----------------------------------------------------------------------
      CHARACTER NAMSTR*36
      INTEGER   IERR, ILUN, IERR2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF ((IERR.LT.1) .OR. (IERR.GT.26)) GO TO 330
         GO TO (110,120,130,140,150,160,170,180,190,200,210,220,230,
     *      240,330,250,260,330,330,330,270,280,290,300,310,320), IERR
 110        WRITE (MSGTXT,1110)
            GO TO 500
 120        WRITE (MSGTXT,1200)
            GO TO 500
 130        WRITE (MSGTXT,1130)
            GO TO 500
 140        WRITE (MSGTXT,1140)
            GO TO 500
 150        WRITE (MSGTXT,1150)
            GO TO 500
 160        WRITE (MSGTXT,1160)
            GO TO 500
 170        WRITE (MSGTXT,1170)
            GO TO 500
 180        WRITE (MSGTXT,1180)
            GO TO 500
 190        WRITE (MSGTXT,1190)
            GO TO 500
 200        WRITE (MSGTXT,1200)
            GO TO 500
 210        WRITE (MSGTXT,1210)
            GO TO 500
 220        WRITE (MSGTXT,1220)
            GO TO 500
 230        WRITE (MSGTXT,1230)
            GO TO 500
 240        WRITE (MSGTXT,1240)
            GO TO 500
 250        WRITE (MSGTXT,1250)
            GO TO 500
 260        WRITE (MSGTXT,1260)
            GO TO 500
 270        WRITE (MSGTXT,1270)
            GO TO 500
 280        WRITE (MSGTXT,1280)
            GO TO 500
 290        WRITE (MSGTXT,1290)
            GO TO 500
 300        WRITE (MSGTXT,1300)
            GO TO 500
 310        WRITE (MSGTXT,1310)
            GO TO 500
 320        WRITE (MSGTXT,1320)
            GO TO 500
 330        WRITE (MSGTXT,1330) IERR
            GO TO 500
 500  CONTINUE
      CALL MSGWRT (7)
      IF (ILUN.EQ.0) GO TO 999
         CALL GTNAME (ILUN, NAMSTR, IERR2)
C                                       Print file name or LUN.
         IF (IERR2.EQ.0) CALL PRTNAM (NAMSTR, 7)
         IF (IERR2.EQ.0) GO TO 999
            WRITE (MSGTXT,1500) ILUN
            CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('FILE NOT OPEN')
 1120 FORMAT ('INPUT PARAMETER ERROR')
 1130 FORMAT ('IO ERROR ("OTHER")')
 1140 FORMAT ('END OF FILE')
 1150 FORMAT ('BEGINNING OF MEDIUM')
 1160 FORMAT ('END OF MEDIUM')
 1170 FORMAT ('BUFFER TOO SMALL')
 1180 FORMAT ('ILLEGAL DATA TYPE')
 1190 FORMAT ('LOGICAL END OF FILE')
 1200 FORMAT ('CATALOG OPERATION ERROR')
 1210 FORMAT ('CATALOG STATUS ERROR')
 1220 FORMAT ('MAP NOT IN CATALOG')
 1230 FORMAT ('EXT FILE NOT IN CATALOG')
 1240 FORMAT ('NO ROOM IN HEADER/CATALOG')
 1250 FORMAT ('ILLEGAL WINDOW SPECIFICATION')
 1260 FORMAT ('ILLEGAL WINDOW SPECIFICATION FOR WRITING')
 1270 FORMAT ('CREATE: FILE ALREADY EXITS')
 1280 FORMAT ('CREATE: VOLUME UNAVAILABLE')
 1290 FORMAT ('CREATE: SPACE UNAVAILABEL')
 1300 FORMAT ('CREATE: "OTHER"')
 1310 FORMAT ('DESTROY: "OTHER"')
 1320 FORMAT ('OPEN: "OTHER"')
 1330 FORMAT ('IO ERROR:',I5)
 1500 FORMAT ('LUN =',I6)
      END
