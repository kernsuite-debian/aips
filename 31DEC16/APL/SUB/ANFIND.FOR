      SUBROUTINE ANFIND (ARRAY, STN, BVAL, TYPE)
C-----------------------------------------------------------------------
C! Recognizes the array or station name, checks By for handedness
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2016
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
C   Routine to recognize the handedness of the Bx-By-Bz antenna system
C   from a list of known array and station names,
C   Inputs:
C      ARRAY   C*8    Array name
C      STN     C*8    Station name
C      BVAL    D(3)   Bx,By,Bz coordinate of station/array center
C   Output:
C      TYPE    I      1 - don't know, 2 - left, 3 - right
C-----------------------------------------------------------------------
      CHARACTER ARRAY*(*), STN*(*)
      INTEGER   TYPE
      DOUBLE PRECISION BVAL(3)
C
      INTEGER   NXPYMP, NXMYMP, NXPYPP, NXMYPP, NXPYMM, NXMYMM, NXPYPM,
     *   NXMYPM
      PARAMETER (NXPYMP=18)
      PARAMETER (NXMYMP=27)
      PARAMETER (NXPYPP=12)
      PARAMETER (NXMYPP=1)
      PARAMETER (NXPYMM=1)
      PARAMETER (NXMYMM=1)
      PARAMETER (NXPYPM=1)
      PARAMETER (NXMYPM=9)
C
      INTEGER   I
      CHARACTER XPYMZP(NXPYMP)*8, XMYMZP(NXMYMP)*8,  XPYPZP(NXPYPP)*8,
     *   XMYPZP(NXMYPP)*8, XPYMZM(NXPYMM)*8, XMYMZM(NXMYMM)*8,
     *   XPYPZM(NXPYPM)*8, XMYPZM(NXMYPM)*8, LSTN*8
      INCLUDE 'INCS:DMSG.INC'
      DATA XPYMZP /'SC','GB','HN','AR','RO','YS','JB','DEF','KNO','DAR',
     *   'MK2','TAB','VLBA_SC','VLBA_HN','PV','PICOVEL','SC-VLBA',
     *   'HN-VLBA'/
      DATA XMYMZP /'BR','FD','KP','LA','NL','OV','PT','Y','MK','GO',
     *   'VLBA_BR','VLBA_FD','VLBA_KP','VLBA_LA','VLBA_NL','VLBA_OV',
     *   'VLBA_PT','VLBA_MK','BR-VLBA','FD-VLBA','KP-VLBA','LA-VLBA',
     *   'NL-VLBA','OV-VLBA','PT-VLBA','MK-VLBA','DSS13'/
      DATA XPYPZP /'EB','EF','NT','WB','UR','MC','ON','CAM','EFLSBERG',
     *   'ONSALA60','PDBURE','CRIMEA'/
      DATA XMYPZP /'SH'/
      DATA XPYMZM /'  '/
      DATA XMYMZM /'  '/
      DATA XPYPZM /'HH'/
      DATA XMYPZM /'AT','PA','PKS','TI','HOB','CD','CED','MOP','CAT'/
C-----------------------------------------------------------------------
      TYPE = 1
      I = LEN (STN)
      I = MIN (I, 8)
      LSTN = STN(:I)
C                                       VLA/EVLA simple case
      IF ((ARRAY.EQ.'VLA') .OR. (ARRAY.EQ.'EVLA')) THEN
         IF (BVAL(2).LT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       ALMA
      ELSE IF (ARRAY.EQ.'ALMA') THEN
         IF (BVAL(2).LT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       ATCA
      ELSE IF ((ARRAY.EQ.'ATCA') .OR. (ARRAY.EQ.'ASKAP')) THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
         ELSE IF (BVAL(2).LT.0.0D0) THEN
            TYPE = 2
            END IF
         IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       GMRT also has array center
      ELSE IF (ARRAY.EQ.'GMRT') THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       MWA (-32T or other)
      ELSE IF (ARRAY(:3).EQ.'MWA') THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       MWA (-32T or other)
      ELSE IF (ARRAY(:4).EQ.'WSRT') THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
      ELSE IF (ARRAY.EQ.'SAO SMA') THEN
         IF (BVAL(2).LT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       Nobeyama Radioheliograph
      ELSE IF (ARRAY(:4).EQ.'NORH') THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
         ELSE
            TYPE = 2
            END IF
         IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       LOFAR
      ELSE IF (ARRAY.EQ.'LOFAR') THEN
         IF (BVAL(2).GT.0.0D0) THEN
            TYPE = 3
            IF (STN(:2).EQ.'UK') TYPE = 2
         ELSE
            TYPE = 2
            IF (STN(:2).EQ.'UK') TYPE = 3
            END IF
         IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
            GO TO 900
         ELSE
            GO TO 999
            END IF
C                                       array name only
      ELSE IF (LSTN.EQ.' ') THEN
C                                       look mainly for station
      ELSE
         IF ((BVAL(1).EQ.0.0D0) .AND. (BVAL(2).EQ.0.0D0) .AND.
     *      (BVAL(3).EQ.0.0D0)) GO TO 900
         DO 110 I = 1,NXPYMP
            IF (LSTN.EQ.XPYMZP(I)) THEN
               IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 110        CONTINUE
         DO 120 I = 1,NXMYMP
            IF (LSTN.EQ.XMYMZP(I)) THEN
               IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 120        CONTINUE
         DO 130 I = 1,NXPYPP
            IF (LSTN.EQ.XPYPZP(I)) THEN
               IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 130        CONTINUE
         DO 140 I = 1,NXMYPP
            IF (LSTN.EQ.XMYPZP(I)) THEN
               IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).LT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 140        CONTINUE
         DO 150 I = 1,NXPYMM
            IF (LSTN.EQ.XPYMZM(I)) THEN
               IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 150        CONTINUE
         DO 160 I = 1,NXMYMM
            IF (LSTN.EQ.XMYMZM(I)) THEN
               IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 160        CONTINUE
         DO 170 I = 1,NXPYPM
            IF (LSTN.EQ.XPYPZM(I)) THEN
               IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).LT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 170        CONTINUE
         DO 180 I = 1,NXMYPM
            IF (LSTN.EQ.XMYPZM(I)) THEN
               IF (BVAL(2).GT.0.0D0) THEN
                  TYPE = 3
               ELSE IF (BVAL(2).LT.0.0D0) THEN
                  TYPE = 2
                  END IF
               IF ((BVAL(1).GT.0.0D0) .OR. (BVAL(3).GT.0.0D0)) THEN
                  GO TO 900
               ELSE
                  GO TO 999
                  END IF
               END IF
 180        CONTINUE
         END IF
      GO TO 999
C
 900  WRITE (MSGTXT,1000) ARRAY, STN, BVAL
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANFIND: ODD ',A8,1X,A8,3(1PE10.3))
      END
