LOCAL INCLUDE 'REBYTE.INC'
      INTEGER   USER, UMAX, NBYWD, LUNI, LUNO, VOLI, VOLO, BUFF(256),
     *   US1, US2
      LOGICAL   T, F, MBLANK
      COMMON /REBPAR/ BUFF, USER, UMAX, NBYWD, LUNI, LUNO, VOLI, VOLO,
     *   T, F, US1, US2, MBLANK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
      PROGRAM REBYTE
C-----------------------------------------------------------------------
C! RECAT converts whole data area from opposite to current byte order
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 2007-2008, 2010, 2015
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
C   A service routine to convert data from one byte order (MACPPC and
C   Solaris) to the other (Linux and Mac Intel) and vice versa.  It
C   assumes that the data are in the wrong byte order for the current
C   computer and changes them.  It writes in a data area separate from
C   the input area.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   TTY(2), IERR
      INCLUDE 'REBYTE.INC'
      DATA TTY /5,0/, PRGNAM /'REBYTE'/
C-----------------------------------------------------------------------
C                                       Standard AIPS inits.
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL REBINI (TTY, IERR)
      IF (IERR.NE.0) GO TO 990
      NVOL = 35
      DO 10 USER = US1,US2
         CALL REBILD (IERR)
         IF (IERR.GT.0) GO TO 990
 10      CONTINUE
C
 990  CALL ACOUNT (2)
C
 999  STOP
      END
      SUBROUTINE REBINI (TTY, IERR)
C-----------------------------------------------------------------------
C   Assign disk areas, init parameters
C   Input:
C      TTY     I(2)   LUN and FTAB index of open TTY.
C   Output:
C      IERR    I      Error code
C-----------------------------------------------------------------------
      INTEGER   TTY(2), IERR
C
      INTEGER   J, JTRIM
      HOLLERITH HNAME(20), HDISK
      CHARACTER FNAME*192, DISKN*4
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
C                                       limit of ehex 3 digit
      UMAX = 36 * 36 * 36 - 1
      NBYWD = NBITWD / 8
C                                       user number range
      WRITE (MSGTXT,1000) UMAX
      CALL INQINT (TTY, MSGTXT, 2, US1, IERR)
      IF ((IERR.NE.0) .OR. (US2.LT.US1)) GO TO 900
      IF ((US1.EQ.0) .AND. (US2.EQ.0)) THEN
         US1 = 1
         US2 = UMAX
         END IF
      US1 = MAX (US1, 1)
      US2 = MAX (US1, MIN (US2, UMAX))
C                                       data area in
      WRITE (MSGTXT,1010) 'input'
      CALL INQSTR (TTY, MSGTXT, 192, FNAME, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR READING FILE NAME'
         IF (IERR.EQ.10) MSGTXT = 'INPUT STRING LONGER THAN 192' //
     *      ' CHARACTERS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      J = JTRIM (FNAME)
      CALL CHR2H (J, FNAME, 1, HNAME)
      DISKN = 'DA0Y'
      CALL CHR2H (4, DISKN, 1, HDISK)
      CALL ZCRLOG (4, HDISK, J, HNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, 'input'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       data area out
      WRITE (MSGTXT,1010) 'output'
      CALL INQSTR (TTY, MSGTXT, 192, FNAME, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR READING FILE NAME'
         IF (IERR.EQ.10) MSGTXT = 'INPUT STRING LONGER THAN 192' //
     *      ' CHARACTERS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      J = JTRIM (FNAME)
      CALL CHR2H (J, FNAME, 1, HNAME)
      DISKN = 'DA0Z'
      CALL CHR2H (4, DISKN, 1, HDISK)
      CALL ZCRLOG (4, HDISK, J, HNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, 'output'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 900  IF ((IERR.EQ.0) .AND. (FNAME.EQ.' ')) IERR = 10
      IF ((IERR.EQ.0) .AND. (US2.LT.US1)) IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter range of user numbers between 1 and',I6)
 1010 FORMAT ('Enter full path name to ',A,' data directory')
 1015 FORMAT ('ERROR',I3,' CREATING ',A,' LOGICAL NAME')
      END
      SUBROUTINE REBILD (IRET)
C-----------------------------------------------------------------------
C   Byte reverse and copy a users data
C   Output:
C      IRET   I      error  code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER PNAMEI*48, PNAMEO*48
      INTEGER   INDI, INDO, SIZEI, SIZEO, NLPR, NWPL, IPOS, IREC, I, I1,
     *   VMAX, IVER
      INCLUDE 'REBYTE.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      LUNI = 15
      LUNO = 16
      VOLI = 34
      VOLO = 35
      T = .TRUE.
      F = .FALSE.
C                                       does this user have files
      CALL PHILEN ('MS', VOLI,  USER, 0, PNAMEI, IRET)
      CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET, 'EXIST', 'MS', 0
         GO TO 990
C                                       copy MS file
      ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
         WRITE (MSGTXT,2000) 'MS', 0, USER
         CALL MSGWRT (2)
         CALL PHILEN ('MS', VOLO,  USER, 0, PNAMEO, IRET)
         CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATE', 'MS', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN IN', 'MS', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUT', 'MS', 0
            GO TO 990
            END IF
         NLPR = 10
         NWPL = 25
         DO 20 IREC = 1,SIZEI
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', 'MS', 0, IREC
               GO TO 990
               END IF
            IF (IREC.EQ.1) CALL ZBFLI2 (NBYWD, 2, 3, BUFF(1), BUFF(1))
            IPOS = 3
            DO 10 I = 1,NLPR
               CALL ZBFLI2 (NBYWD, 3, 3, BUFF(IPOS), BUFF(IPOS))
               IPOS = IPOS + NWPL
 10            CONTINUE
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'MS', 0, IREC
               GO TO 990
               END IF
 20         CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE IN', 'MS', 0
            GO TO 990
            END IF
         CALL ZCLOSE (LUNO, INDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', 'MS', 0
            GO TO 990
            END IF
         END IF
C                                       TG files
      DO 30 IVER = 0,35
         CALL PHILEN ('TG', VOLI, 400, IVER, PNAMEI, IRET)
         CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
         IF (IRET.GT.1) THEN
            WRITE (MSGTXT,1000) IRET, 'EXIST', 'TG', IVER
            GO TO 990
C                                       copy TG file
         ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
            WRITE (MSGTXT,2000) 'TG', IVER, USER
            CALL MSGWRT (2)
            CALL FLPBYT ('TG', 400, IVER, SIZEI, IRET)
            IF (IRET.GT.0) GO TO 999
            END IF
 30      CONTINUE
C                                       SG directory file
      CALL PHILEN ('SG', VOLI, USER, 0, PNAMEI, IRET)
      CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET, 'EXIST', 'SG', 0
         GO TO 990
C                                       copy SG directory
      ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
         WRITE (MSGTXT,2000) 'SG', 0, USER
         CALL MSGWRT (2)
         CALL PHILEN ('SG', VOLO,  USER, 0, PNAMEO, IRET)
         CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATE', 'SG', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN IN', 'SG', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUT', 'SG', 0
            GO TO 990
            END IF
         NWPL = 7
         NLPR = 256 / NWPL
         DO 40 IREC = 1,SIZEI
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', 'SG', 0, IREC
               GO TO 990
               END IF
            IF (IREC.EQ.1) THEN
               CALL ZBFLI2 (NBYWD, 7, 3, BUFF(1), BUFF(1))
               IPOS = 8
               I1 = 2
               VMAX = BUFF(1)
            ELSE
               IPOS = 1
               I1 = 1
               END IF
            DO 35 I = I1,NLPR
               CALL ZBFLI2 (NBYWD, 3, 3, BUFF(IPOS), BUFF(IPOS))
               IPOS = IPOS + NWPL
 35            CONTINUE
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'SG', 0, IREC
               GO TO 990
               END IF
 40         CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE IN', 'SG', 0
            GO TO 990
            END IF
         CALL ZCLOSE (LUNO, INDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', 'SG', 0
            GO TO 990
            END IF
C                                       SG data files
         DO 50 IVER = 1,VMAX
            CALL PHILEN ('SG', VOLI, USER, IVER, PNAMEI, IRET)
            CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
            IF (IRET.GT.1) THEN
               WRITE (MSGTXT,1000) IRET, 'EXIST', 'SG', IVER
               GO TO 990
C                                       copy SG directory
            ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
               WRITE (MSGTXT,2000) 'SG', IVER, USER
               CALL MSGWRT (2)
               CALL FLPBYT ('SG', USER, IVER, SIZEI, IRET)
               IF (IRET.GT.0) GO TO 999
               END IF
 50         CONTINUE
         END IF
C                                       CA file
      IRET = 0
      CALL PHILEN ('CA', VOLI, 0, 0, PNAMEI, IRET)
      CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET, 'EXIST', 'CA', 0
         GO TO 990
C                                       copy CA file
      ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
         WRITE (MSGTXT,2000) 'CA', 0, USER
         CALL MSGWRT (2)
         CALL PHILEN ('CA', VOLO, 0, 0, PNAMEO, IRET)
         CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATE', 'CA', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN IN', 'CA', 0
            GO TO 990
            END IF
         CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUT', 'CA', 0
            GO TO 990
            END IF
         NWPL = 10
         NLPR = 256 / NWPL
         DO 60 IREC = 1,SIZEI
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', 'CA', 0, IREC
               GO TO 990
               END IF
            IF (IREC.EQ.1) THEN
               CALL ZBFLI2 (NBYWD, 15, 3, BUFF(1), BUFF(1))
            ELSE
               IPOS = 1
               DO 55 I = 1,NLPR
                  CALL ZBFLI2 (NBYWD, 5, 3, BUFF(IPOS), BUFF(IPOS))
                  IPOS = IPOS + NWPL
 55               CONTINUE
               END IF
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'CA', 0, IREC
               GO TO 990
               END IF
 60         CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE IN', 'CA', 0
            GO TO 990
            END IF
         CALL ZCLOSE (LUNO, INDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', 'CA', 0
            GO TO 990
            END IF
C                                       process user data files
         CALL UDATA (IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IRET = 0
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' DOING ',A,' ON FILE TYPE ',A,' VER',I4)
 1010 FORMAT ('ERROR',I3,' DOING ',A,' ON FILE TYPE ',A,' VER',I4,
     *   ' REC',I6)
 2000 FORMAT ('Start copying ',A,' vers',I4,' for user',I5)
      END
      SUBROUTINE FLPBYT (TYPE, SEQ, VER, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a data set containing character and binary data but no
C   double precision data forcing byte and word flip.  This one creates
C   output, opens files, and closes them.  FLPIT does the I/O.
C   Inputs
C      TYPE    C*2   File type
C      SEQ     I     Seq (part of file name)
C      VER     I     Ver (part of file name)
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   SEQ, VER, SIZEI, IRET
C
      INTEGER   SIZEO, INDI, INDO
      CHARACTER PNAMEI*48, PNAMEO*48
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
      CALL PHILEN (TYPE, VOLI, SEQ, VER, PNAMEI, IRET)
      CALL PHILEN (TYPE, VOLO, SEQ, VER, PNAMEO, IRET)
      CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUT', TYPE, VER
         GO TO 990
         END IF
      CALL FLPIT (TYPE, VER, INDI, INDO, SIZEI, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', TYPE, VER
         GO TO 990
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLPBYT: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4)
      END
      SUBROUTINE FLPIT (TYPE, VER, INDI, INDO, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a data set containing character and binary data but no
C   double precision data forcing byte and word flip.
C   Inputs
C      TYPE    C*2   File type
C      VER     I     Ver (part of file name)
C      INDI    I     input FTAB pointer
C      INDO    I     output FTAB pointer
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   VER, INDI, INDO, SIZEI, IRET
C
      INTEGER   IREC
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
C                                       do copy
      DO 20 IREC = 1,SIZEI
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
            GO TO 990
            END IF
         CALL ZBFLC2 (4, 256, 3, BUFF, BUFF)
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
            GO TO 990
            END IF
 20      CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FLPIT: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
      END
      SUBROUTINE FULFLP (TYPE, SEQ, VER, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a data set containing binary data but no characters or double
C   precision data forcing byte and word flip.  Treated as MAP files.
C   Inputs
C      TYPE    C*2   File type
C      SEQ     I     Seq (part of file name)
C      VER     I     Ver (part of file name)
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   SEQ, VER, SIZEI, IRET
C
      INTEGER   SIZEO, INDI, INDO, IREC
      CHARACTER PNAMEI*48, PNAMEO*48
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
      CALL PHILEN (TYPE, VOLI, SEQ, VER, PNAMEI, IRET)
      CALL PHILEN (TYPE, VOLO, SEQ, VER, PNAMEO, IRET)
      CALL ZCREAT (VOLO, PNAMEO, SIZEI, T, SIZEO, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUT', TYPE, VER
         GO TO 990
         END IF
      DO 20 IREC = 1,SIZEI
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
            GO TO 990
            END IF
         IF (MBLANK) THEN
            CALL ZBFLM2 (4, 256, 3, BUFF, BUFF)
         ELSE
            CALL ZBFLI2 (4, 256, 3, BUFF, BUFF)
            END IF
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
            GO TO 990
            END IF
 20      CONTINUE
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', TYPE, VER
         GO TO 990
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FULFLP: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4)
 1010 FORMAT ('FULFLP: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
      END
      SUBROUTINE UDATA (IRET)
C-----------------------------------------------------------------------
C   Looks for header files, fixes them, fixes the data file, fixes the
C   extension files.
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   ABYTFL, NMISS, CNO, ITY, I, IPOS, VER, NVER, SIZEI,
     *   SIZEO, INDI, INDO, IREC, NWPL, NLPR
      CHARACTER TYPE*2, PNAMEI*48, PNAMEO*48
      LOGICAL   COMPRS
      INCLUDE 'REBYTE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       look for all possible CB files
      NMISS = 0
      ABYTFL = BYTFLP
      DO 100 CNO = 1,UMAX
         TYPE = 'CB'
         CALL PHILEN (TYPE, VOLI, CNO, 1, PNAMEI, IRET)
         CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
         IF (IRET.GT.1) THEN
            WRITE (MSGTXT,1000) IRET, 'EXIST', TYPE, CNO
            GO TO 990
         ELSE IF ((IRET.EQ.1) .OR. (SIZEI.LE.0)) THEN
            NMISS = NMISS + 1
            IRET = 0
            IF (NMISS.GT.500) GO TO 999
C                                       copy CB file
         ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
            NMISS = 0
            WRITE (MSGTXT,2000) TYPE, CNO, USER
            CALL MSGWRT (2)
            CALL PHILEN (TYPE, VOLO, CNO, 1, PNAMEO, IRET)
            CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CREATE', TYPE, CNO
               GO TO 990
               END IF
            CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN IN', TYPE, CNO
               GO TO 990
               END IF
            CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN OUT', TYPE, CNO
               GO TO 990
               END IF
C                                       header record
            IREC = 1
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, CNO, IREC
               GO TO 990
               END IF
            CALL CATBYT (BUFF, CATBLK)
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, CATBLK, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, CNO, IREC
               GO TO 990
               END IF
C                                       keyword section
            NWPL = 5
            NLPR = 256 / NWPL
            DO 20 IREC = 2,SIZEI
               CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, CNO, IREC
                  GO TO 990
                  END IF
               IPOS = 4
               DO 10 I = 1,NLPR
                  IF ((IREC.EQ.2) .AND. (I.EQ.1)) THEN
                     CALL ZBFLI2 (4, 6, 3, BUFF, BUFF)
                  ELSE
                     CALL ZBFLI2 (4, 1, 3, BUFF(IPOS+2), BUFF(IPOS+2))
                     IF (BUFF(IPOS+2).EQ.1) THEN
                        CALL ZBFLI2 (8, 1, 3, BUFF(IPOS), BUFF(IPOS))
                     ELSE IF (BUFF(IPOS+2).NE.3) THEN
                        CALL ZBFLI2 (4, 2, 3, BUFF(IPOS), BUFF(IPOS))
                        END IF
                     END IF
                  IPOS = IPOS + NWPL
 10               CONTINUE
               CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, CNO, IREC
                  GO TO 990
                  END IF
 20            CONTINUE
            CALL ZCLOSE (LUNI, INDI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOSE IN', TYPE, CNO
               GO TO 990
               END IF
            CALL ZCLOSE (LUNO, INDO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', TYPE, CNO
               GO TO 990
               END IF
C                                       data file
            CALL H2CHR (2, KHPTYO, CATH(KHPTY), TYPE)
            WRITE (MSGTXT,2000) TYPE, CNO, USER
            CALL MSGWRT (2)
            CALL PHILEN (TYPE, VOLI, CNO, 1, PNAMEI, IRET)
            CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
            IF (IRET.GT.1) THEN
               WRITE (MSGTXT,1000) IRET, 'EXIST', TYPE, CNO
               GO TO 990
C                                       copy data file
            ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
               MBLANK = .FALSE.
               IF ((TYPE.EQ.'MA') .AND. (CATR(KRBLK).EQ.FBLANK)) MBLANK
     *            = .TRUE.
               COMPRS = (TYPE.EQ.'UV') .AND. (CATBLK(KINAX).EQ.1)
               IF (COMPRS) THEN
                  CALL CMPFLP (CNO, 1, SIZEI, IRET)
               ELSE
                  CALL FULFLP (TYPE, CNO, 1, SIZEI, IRET)
                  END IF
               IF (IRET.NE.0) GO TO 999
            ELSE
               MSGTXT = 'WARNING: DATA FILE DOES NOT SEEM TO EXIST'
               CALL MSGWRT (7)
               END IF

C                                       extension files
            DO 50 ITY = 1,KIEXTN
               CALL H2CHR (2, 1, CATH(KHEXT+ITY-1), TYPE)
               NVER = CATBLK(KIVER+ITY-1)
               IF ((TYPE(1:1).LT.'A') .OR. (TYPE(2:2).LT.'A')) NVER = 0
               IF ((TYPE(1:1).GT.'Z') .OR. (TYPE(2:2).GT.'Z')) NVER = 0
               DO 40 VER = 1,NVER
                  CALL PHILEN (TYPE, VOLI, CNO, VER, PNAMEI, IRET)
                  CALL ZEXIST (VOLI, PNAMEI, SIZEI, IRET)
                  IF (IRET.GT.1) THEN
                     WRITE (MSGTXT,1000) IRET, 'EXIST', TYPE, CNO
                     GO TO 990
C                                       copy ext file
                  ELSE IF ((IRET.EQ.0) .AND. (SIZEI.GT.0)) THEN
                     WRITE (MSGTXT,2010) TYPE, CNO, VER, USER
                     CALL MSGWRT (2)
                     CALL PHILEN (TYPE, VOLO, CNO, VER, PNAMEO, IRET)
                     CALL ZCREAT (VOLO, PNAMEO, SIZEI, F, SIZEO, IRET)
                     IF (IRET.GT.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'CREATE', TYPE, CNO
                        GO TO 990
                        END IF
                     CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'OPEN IN', TYPE, CNO
                        GO TO 990
                        END IF
                     CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'OPEN OUT', TYPE, CNO
                        GO TO 990
                        END IF
C                                       HIstory file
                     IF (TYPE.EQ.'HI') THEN
                        DO 30 IREC = 1,SIZEI
                           CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE,
     *                           CNO, IREC
                              GO TO 990
                              END IF
                           CALL ZBFLI2 (4, 4, 3, BUFF, BUFF)
                           CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1010) IRET, 'WRITE OUT',
     *                           TYPE, CNO, IREC
                              GO TO 990
                              END IF
 30                        CONTINUE
C                                       PLot files
                     ELSE IF (TYPE.EQ.'PL') THEN
                        CALL FLIPPL (VER, INDI, INDO, SIZEI, IRET)
                        IF (IRET.GT.0) GO TO 999
C                                       SLice files
                     ELSE IF (TYPE.EQ.'SL') THEN
                        CALL FLIPSL (VER, INDI, INDO, SIZEI, IRET)
                        IF (IRET.GT.0) GO TO 999
C                                       tables
                     ELSE
                        CALL FLPTAB (TYPE, VER, INDI, INDO, SIZEI, IRET)
                        IF (IRET.GT.0) GO TO 999
                        END IF
                     CALL ZCLOSE (LUNI, INDI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'CLOSE IN', TYPE, CNO
                        GO TO 990
                        END IF
                     CALL ZCLOSE (LUNO, INDO, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', TYPE, CNO
                        GO TO 990
                        END IF
                     END IF
 40               CONTINUE
 50            CONTINUE
            END IF
 100     CONTINUE
      IRET = 0
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UDATA: ERROR',I3,' DOING ',A,' ON TYPE ',A,' CNO',I4)
 1010 FORMAT ('UDATA: ERROR',I3,' DOING ',A,' ON TYPE ',A,' CNO',I4,
     *   ' REC',I6)
 2000 FORMAT ('Start copying ',A,' cno',I4,' for user',I5)
 2010 FORMAT ('Start copying ',A,' cno',I4,' ver',I4,' for user',I5)
      END
      SUBROUTINE CMPFLP (SEQ, VER, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a compressed uv data set containing binary data of float and
C   16-bit integer forms.
C   Inputs
C      SEQ     I     Seq (part of file name)
C      VER     I     Ver (part of file name)
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      INTEGER   SEQ, VER, SIZEI, IRET
C
      INTEGER   SIZEO, INDI, INDO, IREC, NP, NV, NC, IP, I, J, LP, LV
      CHARACTER PNAMEI*48, PNAMEO*48, TYPE*2
      LOGICAL   RANDUM
      INCLUDE 'REBYTE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       parameters
      NP = CATBLK(KIPCN)
      NV = 1
      J = CATBLK(KIDIM)
      DO 10 I = 1,J
         NV = NV * CATBLK(KINAX+I-1)
 10      CONTINUE
C                                       open files
      TYPE = 'UV'
      CALL PHILEN (TYPE, VOLI, SEQ, VER, PNAMEI, IRET)
      CALL PHILEN (TYPE, VOLO, SEQ, VER, PNAMEO, IRET)
      CALL ZCREAT (VOLO, PNAMEO, SIZEI, T, SIZEO, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNI, INDI, VOLI, PNAMEI, F, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZOPEN (LUNO, INDO, VOLO, PNAMEO, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUT', TYPE, VER
         GO TO 990
         END IF
      IP = 1
      RANDUM = .TRUE.
      LP = NP
      LV = NV
      DO 30 IREC = 1,SIZEI
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
            GO TO 990
            END IF
         IP = 1
 20      IF (IP.LT.257) THEN
            IF (RANDUM) THEN
               NC = MIN (257-IP, LP)
               CALL ZBFLI2 (4, NC, 3, BUFF(IP), BUFF(IP))
               LP = LP - NC
               IP = IP + NC
               IF (LP.LE.0) THEN
                  LV = NV
                  RANDUM = .FALSE.
                  END IF
            ELSE
               NC = MIN (257-IP, LV)
               J = 2 * NC
               CALL ZBFLI2 (2, J, 3, BUFF(IP), BUFF(IP))
               LV = LV - NC
               IP = IP + NC
               IF (LV.LE.0) THEN
                  LP = NP
                  RANDUM = .TRUE.
                  END IF
               END IF
            GO TO 20
            END IF
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
            GO TO 990
            END IF
 30      CONTINUE
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE IN', TYPE, VER
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE OUT', TYPE, VER
         GO TO 990
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FULFLP: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4)
 1010 FORMAT ('FULFLP: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
      END
      SUBROUTINE FLPTAB (TYPE, VER, INDI, INDO, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a table extension file doing all needed byte flips
C   Inputs
C      TYPE    C*2   File type
C      VER     I     Ver (part of file name)
C      INDI    I     input FTAB pointer
C      INDO    I     output FTAB pointer
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   VER, INDI, INDO, SIZEI, IRET
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   IREC, HEAD(256), DATP(128,2), NROW, MREC, FREC, NWORD,
     *   NDWORD, NFWORD, NCWORD, NSWORD, IROW, I, IPOS, BBUF(UVBFSS), J,
     *   L
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
C                                       header
      IREC = 1
      CALL ZFIO ('READ', LUNI, INDI, IREC, HEAD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
         GO TO 990
         END IF
C                                       flip non-character portions
      CALL ZBFLI2 (4, 16, 3, HEAD(1), HEAD(1))
      CALL ZBFLI2 (4, 8, 3, HEAD(31), HEAD(31))
      CALL ZBFLI2 (4, 13, 3, HEAD(41), HEAD(41))
      CALL ZBFLI2 (4, 44, 3, HEAD(57), HEAD(57))
      CALL ZBFLI2 (4, 128, 3, HEAD(129), HEAD(129))
      CALL ZFIO ('WRIT', LUNO, INDO, IREC, HEAD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
         GO TO 990
         END IF
      NROW = HEAD(5)
C                                       pointers
      IREC = 2
      CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
         GO TO 990
         END IF
      CALL ZBFLI2 (4, 256, 3, BUFF, DATP)
      CALL ZFIO ('WRIT', LUNO, INDO, IREC, DATP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
         GO TO 990
         END IF
      MREC = HEAD(49) - 1
C                                       copy selection, titles, units
      DO 10 IREC = 3,MREC
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
            GO TO 990
            END IF
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
            GO TO 990
            END IF
 10      CONTINUE
C                                       keywords
      FREC = HEAD(49)
      MREC = HEAD(50) - 1
      DO 30 IREC = FREC,MREC
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
            GO TO 990
            END IF
         IPOS = 3
         DO 20 I = 1,51
            CALL ZBFLI2 (4, 1, 3, BUFF(IPOS+2), BUFF(IPOS+2))
            IF (BUFF(IPOS+2).EQ.1) THEN
               CALL ZBFLI2 (8, 1, 3, BUFF(IPOS), BUFF(IPOS))
            ELSE IF (BUFF(IPOS+2).NE.3) THEN
               CALL ZBFLI2 (4, 2, 3, BUFF(IPOS), BUFF(IPOS))
               END IF
            IPOS = IPOS + 5
 20         CONTINUE
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
            GO TO 990
            END IF
 30      CONTINUE
C                                       how many doubles
      NWORD = HEAD(8)
      NDWORD = 0
      NFWORD = 0
      NCWORD = 0
      DO 40 I = 1,HEAD(10)
         J = MOD (DATP(I,2), 10)
         L = (DATP(I,2) - J) / 10
         IF (J.EQ.1) THEN
            NDWORD = NDWORD + L
         ELSE IF (J.EQ.2) THEN
            NFWORD = NFWORD + L
         ELSE IF (J.EQ.3) THEN
            NCWORD = NCWORD + (L-1) / 4 + 1
            END IF
 40      CONTINUE
      NSWORD = NWORD - 2 * NDWORD - NFWORD - NCWORD
      FREC = HEAD(50)
C                                       1+ records / buffer
      IF (HEAD(9).GT.0) THEN
         DO 70 IREC = FREC,SIZEI
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
               GO TO 990
               END IF
            IPOS = 1
            DO 60 I = 1,HEAD(9)
               IF (NDWORD.GT.0) CALL ZBFLI2 (8, NDWORD, 3, BUFF(IPOS),
     *            BUFF(IPOS))
               IPOS = IPOS + 2 * NDWORD
               IF (NFWORD.GT.0) CALL ZBFLI2 (4, NFWORD, 3, BUFF(IPOS),
     *            BUFF(IPOS))
               IPOS = IPOS + NFWORD + NCWORD
               IF (NSWORD.GT.0) CALL ZBFLI2 (4, NSWORD, 3, BUFF(IPOS),
     *            BUFF(IPOS))
               IPOS = IPOS + NSWORD
 60            CONTINUE
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', TYPE, VER, IREC
               GO TO 990
               END IF
 70         CONTINUE
C                                       > 1 buffer / record
      ELSE
         IREC = FREC - 1
         DO 80 IROW = 1,NROW
            IPOS = 1
            DO 75 I = 1,-HEAD(9)
               CALL ZFIO ('READ', LUNI, INDI, IREC+I, BBUF(IPOS), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'READ IN', TYPE, VER, IREC
                  GO TO 990
                  END IF
               IPOS = IPOS + 256
 75            CONTINUE
            IPOS = 1
            IF (NDWORD.GT.0) CALL ZBFLI2 (8, NDWORD, 3, BBUF(IPOS),
     *         BBUF(IPOS))
            IPOS = IPOS + 2 * NDWORD
            IF (NFWORD.GT.0) CALL ZBFLI2 (4, NFWORD, 3, BBUF(IPOS),
     *         BBUF(IPOS))
            IPOS = IPOS + NFWORD + NCWORD
            IF (NSWORD.GT.0) CALL ZBFLI2 (4, NSWORD, 3, BBUF(IPOS),
     *         BBUF(IPOS))
            IPOS = 1
            DO 76 I = 1,-HEAD(9)
               CALL ZFIO ('WRIT', LUNO, INDO, IREC+I, BBUF(IPOS), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'WROTE OUT', TYPE, VER, IREC
                  GO TO 990
                  END IF
               IPOS = IPOS + 256
 76            CONTINUE
 80         CONTINUE
         IREC = IREC - HEAD(9)
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FLPTAB: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
      END
      SUBROUTINE PHILEN (TYPE, IVOL, NSEQ, IVER, PNAM, IERR)
C-----------------------------------------------------------------------
C   Construct a physical file name in PNAM from TYPE, IVOL, NSEQ, and
C   IVER - either for public data files or user-specific files.
C   REBYTE version: uses USER not NLUSER
C   Inputs:
C      TYPE    C*2   Type of file: e.g. 'MA' for map file
C      IVOL    I     Number of the disk volume to be used (1-15)
C      NSEQ    I     Sequence number (000-4095)
C      IVER    I     Version number (00-255)
C   Outputs:
C      PNAM    C*48  physical file name, left justified
C      IERR    I     Error return code: 0 = good return.   1 = error.
C
C   Example: If TYPE='MA', IVOL=7, AIPSVER=C, NSEQ=321, IVER=12799,
C            NLUSER=762 then
C               PNAME='DA06:MAD08X9VJ;'      for public data or
C               PNAME='DA06:MAD08X9VJ.0L6;'  for private data
C           use base 36 now: 321 => 08X, 12799 => 9VJ, 762 => 0L6
C           disks are base 36 but 0 relative
C   TYPE = 'MT' leads to special name for tapes
C   TYPE = 'TK' leads to special name for TEK4012 plotter CRT
C   TYPE = 'TV' leads to special name for TV device
C   TYPE = 'ME' leads to special logical for POPS memory files
C
C   Generic version - the ZOPEN, ZTRLOG, etc. routines interpret the
C   resulting VMS-like names.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, PNAM*48
      INTEGER   IVOL, NSEQ, IVER, IERR
C
      INTEGER   IVOL2, NUNAME, UNAME, VERLIM, VOLLIM, CATLIM
      CHARACTER JUNK*4, UNAMES(15)*2, VERDAT*1, VERSYS*1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'REBYTE.INC'
      DATA NUNAME, UNAMES /15, 'AC','BA','BQ','HE','IC','ID','IN','ME',
     *   'SP','TP','TD','GR','PW','MT','TC'/
C                                       Current AIPS data version level
      DATA VERDAT, VERSYS /'D','D'/
C-----------------------------------------------------------------------
      IERR = 1
      PNAM = 'DA00:MADFFFFFF; '
      PNAM(8:8) = VERDAT
      IF (TYPE.EQ.'MT') PNAM(1:2) = 'MT'
C                                       Special code for TEK 4012
      IF (TYPE.EQ.'TK') THEN
         IF (NTKDEV.LE.0) GO TO 999
         PNAM = 'TKDEV  :'
         CALL ZEHEX (NTKDEV, 2, PNAM(6:7))
C                                       TV device
      ELSE IF (TYPE.EQ.'TV') THEN
         IF (NTVDEV.LE.0) GO TO 999
         PNAM = 'TVDEV  :'
         CALL ZEHEX (NTVDEV, 2, PNAM(6:7))
C                                       Special POPS memory files
      ELSE IF ((TYPE.EQ.'ME') .AND. (IVER.EQ.0)) THEN
         PNAM = VERNAM(1:3) // 'MEM:ME1000000;'
         PNAM(10:10) = VERSYS
C                                       other devices (disk files)
      ELSE
C                                       Check input parameters.
         VERLIM = 36 * 36 * 36 - 1
         CATLIM = VERLIM
         VOLLIM = 35
         IF ((IVOL.LT.1) .OR. (IVOL.GT.VOLLIM)) GO TO 999
         IF ((NSEQ.LT.0) .OR. (NSEQ.GT.CATLIM)) GO TO 999
         IF ((IVER.LT.0) .OR. (IVER.GT.VERLIM)) GO TO 999
C                                       User private?
            UNAME = 0
            IF (NUNAME.LE.0) GO TO 45
               DO 40 UNAME = 1,NUNAME
                  IF (TYPE.EQ.UNAMES(UNAME)) GO TO 45
 40               CONTINUE
               UNAME = 0
C                                       Adjust for 1 relative input
 45         IVOL2 = IVOL - 1
            IF (UNAME.LE.0) IVOL2 = IVOL
            CALL ZEHEX (IVOL2, 4, JUNK)
            PNAM(4:4) = JUNK(4:4)
            PNAM(6:7) = TYPE
            IF (UNAME.LE.0) THEN
               PNAM(8:8) = VERDAT
            ELSE
               PNAM(8:8) = VERSYS
               END IF
            CALL ZEHEX (NSEQ, 4, JUNK)
            PNAM(9:11) = JUNK(2:4)
            CALL ZEHEX (IVER, 4, JUNK)
            PNAM(12:14) = JUNK(2:4)
C                                       User private additions
            IF ((UNAME.LE.0) .AND. (UCTSIZ.GT.0)) THEN
               CALL ZEHEX (USER, 4, JUNK)
               PNAM(15:) = '.' // JUNK(2:4) // ';'
               END IF
         END IF
C                                       No error
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE CATBYT (INB, OUT)
C-----------------------------------------------------------------------
C   Coverts a regular header by byte/word/double swap only
C   Inputs:
C      INB     I(256)   in buffer
C   Outputs:
C      OUT    I(256)   out buffer
C   If header format changes this may have to change
C-----------------------------------------------------------------------
      INTEGER   INB(256), OUT(256)
C
      INTEGER   I, IP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PHDR.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, INB, OUT)
C                                       CRV
      I = 2 * KDCRV - 1
      CALL ZBFLI2 (8, KICTPN, 3, OUT(I), OUT(I))
C                                       CRP, CRT, EPO, DMX, DMN
      IP = 3 * KICTPN + 3
      CALL ZBFLI2 (4, IP, 3, OUT(KRCIC), OUT(KRCIC))
C                                       BLK
      CALL ZBFLM2 (4, 1, 3, OUT(KRBLK), OUT(KRBLK))
C                                       GCN, PCN, DIM, NAX, IMS
      IP = 3 + KICTPN + 1
      CALL ZBFLI2 (4, IP, 3, OUT(KIGCN), OUT(KIGCN))
C                                       IMU, ALT, BMJ, BMN, BPA, NIT
      IP = 6
      CALL ZBFLI2 (4, IP, 3, OUT(KIIMU), OUT(KIIMU))
C                                       ORA, ODE, RST, ARV
      I = 2 * KDORA - 1
      CALL ZBFLI2 (8, 4, 3, OUT(I), OUT(I))
C                                       ARP, XSH, YSH
      IP = 3
      CALL ZBFLI2 (4, IP, 3, OUT(KRARP), OUT(KRARP))
C                                       VER
      CALL ZBFLI2 (4, KIEXTN, 3, OUT(KIVER), OUT(KIVER))
C
 999  RETURN
      END
      SUBROUTINE FLIPSL (VER, INDI, INDO, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a SLice file containing character and binary data but no
C   double precision data forcing byte and word flip, skipping
C   holleriths
C   Inputs
C      VER     I     Ver (part of file name)
C      INDI    I     input FTAB pointer
C      INDO    I     output FTAB pointer
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      INTEGER   VER, INDI, INDO, SIZEI, IRET
C
      INTEGER   IREC
      INCLUDE 'REBYTE.INC'
C-----------------------------------------------------------------------
C                                       do copy
      DO 20 IREC = 1,SIZEI
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', 'SL', VER, IREC
            GO TO 990
            END IF
         IF (IREC.EQ.1) THEN
            CALL ZBFLI2 (4, 7, 3, BUFF(1), BUFF(1))
            CALL ZBFLI2 (4, 6, 3, BUFF(11), BUFF(11))
            CALL ZBFLI2 (4, 1, 3, BUFF(29), BUFF(29))
            CALL ZBFLI2 (4, 224, 3, BUFF(33), BUFF(33))
         ELSE IF (IREC.EQ.2) THEN
            CALL ZBFLI2 (4, 8, 3, BUFF(4), BUFF(4))
            CALL ZBFLI2 (4, 240, 3, BUFF(17), BUFF(17))
         ELSE
            CALL ZBFLI2 (4, 256, 3, BUFF, BUFF)
            END IF
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'SL', VER, IREC
            GO TO 990
            END IF
 20      CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FLIPSL: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
      END
      SUBROUTINE FLIPPL (VER, INDI, INDO, SIZEI, IRET)
C-----------------------------------------------------------------------
C   Copies a PLot file containing character and binary data but no
C   double precision data forcing byte and word flip, skipping
C   holleriths
C   Inputs
C      VER     I     Ver (part of file name)
C      INDI    I     input FTAB pointer
C      INDO    I     output FTAB pointer
C      SIZEI   I     Size of input file in 256-word records
C   Outputs
C      IRET    I     Error code
C-----------------------------------------------------------------------
      INTEGER   VER, INDI, INDO, SIZEI, IRET
C
      INTEGER   IREC, NREC, IOPOS, I, OP(2), NWD(18), IWD, NERR
      INCLUDE 'REBYTE.INC'
      DATA NWD /6, 20, 5, 3, 3, 5, 3, 2, 2, 3, 3, 12, 8, 5, 5, 4, 3, 2/
C-----------------------------------------------------------------------
      NERR = 0
C                                       header section
      IREC = 1
      CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'READ IN', 'PL', VER, IREC
         GO TO 990
         END IF
      CALL ZBFLI2 (4, 1, 3, BUFF(10), I)
      NREC = (I + 9) / 256 + 1
      DO 10 IREC = 1,NREC
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', 'PL', VER, IREC
            GO TO 990
            END IF
         CALL ZBFLC2 (4, 256, 3, BUFF, BUFF)
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'PL', VER, IREC
            GO TO 990
            END IF
 10      CONTINUE
      IOPOS = 9999
      IREC = NREC
C                                       loop through plot commands
 50   OP(1) = 0
      IF (IOPOS.LE.255) CALL ZBFLI2 (4, 2, 3, BUFF(IOPOS), OP)
C                                       write last record
      IF (OP(1).EQ.0) THEN
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'PL', VER, IREC
            GO TO 990
            END IF
         IF (IREC.GE.SIZEI) GO TO 999
C                                       new record
         IREC = IREC + 1
         CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ IN', 'PL', VER, IREC
            GO TO 990
            END IF
         IOPOS = 1
         CALL ZBFLI2 (4, 2, 3, BUFF(IOPOS), OP)
         END IF
C                                       deal with different rec types
C                                       grey-scale or color pixels
      IF ((OP(1).EQ.7) .OR. (OP(1).EQ.11)) THEN
         IWD = NWD(OP(1))
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
         IOPOS = IOPOS + IWD
         IF (OP(1).EQ.11) OP(2) = 3 * OP(2)
 60      IWD = OP(2)
C                                       convert & write buffer
         IF (IWD+IOPOS.GT.257) THEN
            IWD = 257 - IOPOS
            CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'PL', VER, IREC
               GO TO 990
               END IF
            IREC= IREC + 1
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', 'PL', VER, IREC
               GO TO 990
               END IF
            OP(2) = OP(2) - IWD
            IOPOS = 1
            GO TO 60
            END IF
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
C                                       character data
      ELSE IF ((OP(1).EQ.6) .OR. (OP(1).EQ.14) .OR. (OP(1).EQ.15)
     *   .OR. (OP(1).EQ.18)) THEN
         IWD = NWD(OP(1))
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
         IWD = IWD + (OP(2) - 1) / 4 + 1
C                                       misc words
      ELSE IF (OP(1).EQ.8) THEN
         IWD = NWD(OP(1)) + OP(2)
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
C                                       rest legal ops
      ELSE IF ((OP(1).GE.1) .AND. (OP(1).LE.18)) THEN
         IWD = NWD(OP(1))
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
C                                       file end op
      ELSE IF (OP(1).EQ.32767) THEN
         IWD = 257 - IOPOS
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
         CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'PL', VER, IREC
            GO TO 990
            END IF
C                                       any other records?
         NREC = IREC + 1
         DO 70 IREC = NREC,SIZEI
            CALL ZFIO ('READ', LUNI, INDI, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'READ IN', 'PL', VER, IREC
               GO TO 990
               END IF
            CALL ZBFLC2 (4, 256, 3, BUFF, BUFF)
            CALL ZFIO ('WRIT', LUNO, INDO, IREC, BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'WRITE OUT', 'PL', VER, IREC
               GO TO 990
               END IF
 70         CONTINUE
         GO TO 999
C                                       oh no!
      ELSE
         WRITE (MSGTXT,1070) OP, IREC, IOPOS
         CALL MSGWRT (7)
         NERR = NERR + 1
         IWD = 2
         CALL ZBFLI2 (4, IWD, 3, BUFF(IOPOS), BUFF(IOPOS))
         IF (NERR.GT.10) THEN
            IRET = 100
            GO TO 999
            END IF
         END IF
      IOPOS = IOPOS + IWD
      GO TO 50
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FLIPPL: ERROR',I3,' DOING ',A,' ON TYPE ',A,' VER',I4,
     *   ' REC',I6)
 1070 FORMAT ('FLIPPL: BAD OP',2I10,' AT REC, WD',I8,I4)
      END
