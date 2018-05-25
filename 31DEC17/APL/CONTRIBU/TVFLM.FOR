      PROGRAM TVFLM
C-----------------------------------------------------------------------
C! Screen saver for queueing to Dicomed
C# TV-appl
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
C
C                ---------------------------------
C                !                               !
C                !           T V F L M           !
C                !                               !
C                ! Version 3.0 - 14 February 1990!
C                !                               !
C                !          Arnold Rots          !
C                !                               !
C                ---------------------------------
C
C
C     TVFLM saves the TV screen in a Dicomed Image File and queues it.
C
C     Version 3.0: New task (new generation)  -  900214 ahr
C
C     INPUT ADVERBS :
C   COPIES          0.0        200.0    Number of copies requested
C   STRA1                               Specifies film size and type
C   OUTPRINT                            User comment
C
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      REAL      CHAN(14), COPIES, STRA1, OUTPRT(12)
      INTEGER   NPARM, NRET, ERROR, POTERR
      INTEGER   BUFF(256)
      LOGICAL   QUICK
      EQUIVALENCE (CHAN(1), COPIES), (CHAN(2), STRA1),
     *            (CHAN(3), OUTPRT(1))
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NPARM /14/, PRGNAM /'TVFLM '/
C-----------------------------------------------------------------------
C                                       I/O handling
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE., BUFF)
C                                       get inputs
      CALL GTPARM (PRGNAM, NPARM, QUICK, CHAN, BUFF, ERROR)
      IF (ERROR .NE. 0) GO TO 900
      QUICK = .FALSE.
      CALL YTVCIN
C                                       open TV
      CALL TVDICO (POTERR, COPIES, STRA1, OUTPRT)
      IF (POTERR .NE. 0) GOTO 900
C                                       done
      NRET = 0
      GO TO 990
C                                       error exit
 900  CONTINUE
C     WRITE (MSGTXT,9000) SUBNOM(NSUB), ERROR
C     CALL MSGWRT (8)
      NRET = 16
C                                       exit
 990  CALL DIETSK (NRET, QUICK, BUFF)
C
      STOP
C-----------------------------------------------------------------------
C9000 FORMAT (A6,'  ERROR NO',I6)
      END
      SUBROUTINE TVDICO (POTERR, COPIES, STRA1, OUTPRT)
C
C           +-----------------------------------+
C           |                                   |
C           |            t v d i c o            |
C           |                                   |
C           |  Version 3.0  -  14 February 1990 |
C           |                                   |
C           |            Arnold Rots            |
C           |                                   |
C           +-----------------------------------+
C
C     TVDICO is the routine that queues the TV image to the film writer.
C
C     Version 3.0: New generation  -  900214 ahr
C     Input params:
C       copies    I     Number of copies to be made
C       stra1     C(4)  String containing codes for film type and size
C       outprt    C(48) User comment string
C     Output:
C       poterr    I     Return error code
C
C-----------------------------------------------------------------------
C
      REAL COPIES, STRA1, OUTPRT(12), SCL
      INTEGER N1, BUFFER(4096), CONS(8), OUTSEL(8), LUNOUT, LUNIC(3)
      INTEGER NX, NY, IERROR, I, J, K, NPLNON, NPON(12), FLMSIZ, FLMTYP
      INTEGER NCOPY, NSCRPL, NSPLBT, GRON, N16, BITS(16), BLOTCH,
     *        STATUS
      INTEGER N, BITPIX, LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR, DATPTR
      INTEGER USERNO, IC, MONOFF(12), SYSTEM, POTERR, INTSCA
      INTEGER*2 I2, BUFFR2(256), ZOR
      BYTE I1, A1(2), BUFFR1(4096)
      CHARACTER*4 REED, RITE, FILNAM*18, FILM1, FILM2*6, DATEX*9
      CHARACTER*8 TIMEX, STRING*50, TSTR*2
      LOGICAL F, T, GBLOT, FILOPN, TVOPN, SUCCES, BW
C
C     INCLUDE 'INCS:DAPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
C
      INTEGER BUFPTR
      BYTE OUTBUF(8192), LINBUF(4096)
      COMMON /DICCOM/ BUFPTR, OUTBUF
      EQUIVALENCE (OUTBUF(1), LINBUF(1))
C
      EQUIVALENCE (I2,A1(1)), (I1, A1(2))
      EQUIVALENCE (BUFFER(1), BUFFR2(1))
C
      DATA N1, N16, F, T, REED, RITE
     *     /1, 16, .FALSE., .TRUE., 'READ', 'WRIT'/
      DATA MONOFF
     *     /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
C
C-----------------------------------------------------------------------
C                                       Initialize
      BUFPTR = 0
      IERROR = 0
      DO 5 I=1,8
         CONS(I) = 0
         OUTSEL(I) = 9
 5    CONTINUE
      GBLOT = F
      FILOPN = F
      TVOPN = F
      SUCCES = F
      LUNOUT = 101
      LUNIC(1) = 102
      LUNIC(2) = 103
      LUNIC(3) = 104
      NCOPY = COPIES
      STRING = ' '
      CALL H2CHR (48, 1, OUTPRT, STRING)
      CALL H2CHR (2, 1, STRA1, TSTR)
      IF (TSTR .EQ. '35') THEN
            FILM1 = '35mm'
         ELSE IF (TSTR .EQ. '  ') THEN
            FILM1 = '35mm'
         ELSE IF (TSTR .EQ. '2"') THEN
            FILM1 = '2x2'
         ELSE IF (TSTR .EQ. '4"') THEN
            FILM1 = '4x5'
         ELSE
            FILM1 = ' '
      END IF
      CALL H2CHR (2, 3, STRA1, TSTR)
      IF (TSTR .EQ. 'CP') THEN
            FILM2 = 'ColPos'
         ELSE IF (TSTR .EQ. '  ') THEN
            FILM2 = 'ColPos'
         ELSE IF (TSTR .EQ. 'CN') THEN
            FILM2 = 'ColNeg'
         ELSE IF (TSTR .EQ. 'BW') THEN
            FILM2 = 'B/W'
         ELSE
            FILM2 = ' '
      END IF
      USERNO = NLUSER
C
C                                       Construct file name
C
      CALL TIME (TIMEX)
      CALL DATE (DATEX)
      CALL IDATE (I, J, K)
      WRITE (FILNAM,1000) USERNO
 1000 FORMAT (I4)
      FILNAM(5:5) = '_'
      J = J + MONOFF(I)
      IF (((K/4)*4 .EQ. K) .AND. (I .GT. 2)) J = J + 1
      WRITE (FILNAM(6:8),1010) J
 1010 FORMAT (I3)
      FILNAM(9:9) = '_'
      FILNAM(10:11) = TIMEX(1:2)
      FILNAM(12:13) = TIMEX(4:5)
      FILNAM(14:15) = TIMEX(7:8)
      FILNAM(16:18) = '_V1'
      DO 7 I=1,18
         IF (FILNAM(I:I) .EQ. ' ') FILNAM(I:I) = '0'
 7    CONTINUE
C
C                                       Open TV
C
      CALL TVOPEN (BUFFER, IERROR)
      IF (IERROR .NE. 0) GOTO 910
      TVOPN = T
C     IF (MAXINT .NE. 255) GOTO 980
      INTSCA = MAX0(1, (MAXINT+2)/256)
      NX = MAXXTV(1)
      NY = MAXXTV(2)
C
C                                       Open output image file
C
      OPEN (UNIT=LUNOUT, FILE=FILNAM, STATUS='NEW', FORM='UNFORMATTED',
     *      RECL=4096, RECORDTYPE='FIXED', ERR=961)
      FILOPN = T
C
C                                       What's on?
C
      CALL YSPLIT (REED, I, J, NPON(1), NPON(5), NPON(9), F, IERROR)
      IF (IERROR .NE. 0) GOTO 950
      NPLNON = 0
      DO 10 I=1,12
         NPLNON = NPLNON .OR. NPON(I)
 10   CONTINUE
C
C                                       Set film options
C
      IF ((FILM1 .EQ. '35mm') .OR. (FILM1 .EQ. '35MM')) THEN
            FLMSIZ = 1
         ELSE IF ((FILM1 .EQ. '2x2') .OR. (FILM1 .EQ. '2X2')) THEN
            FLMSIZ = 2
         ELSE IF ((FILM1 .EQ. '4x5') .OR. (FILM1 .EQ. '4X5')) THEN
            FLMSIZ = 3
         ELSE
            GOTO 920
      END IF
      IF ((FILM2 .EQ. 'ColPos') .OR. (FILM2 .EQ. 'COLPOS')) THEN
            FLMTYP = 1
            BW = F
         ELSE IF ((FILM2 .EQ. 'ColNeg') .OR. (FILM2 .EQ. 'COLNEG')) THEN
            FLMTYP = 2
            BW = F
         ELSE IF ((FILM2 .EQ. 'b/w') .OR. (FILM2 .EQ. 'B/W')) THEN
            FLMTYP = 3
            BW = T
         ELSE
            GOTO 930
      END IF
      NCOPY = MAX0 (1, NCOPY)
C
C                                       Check hardware options
C                                       Case 1: copy screen
C
      IF ((TVALUS .GT. 0) .AND. (NGRAY .GT. 1)) THEN
C                                       Scratch plane available?
            NSCRPL = NGRAY
            NSPLBT = 2**(NSCRPL-1)
            IF (NPLNON .GE. NSPLBT) GOTO 940
            CALL YCINIT (NSCRPL, BUFFER)
C                                       Handle graphics
C                                       Who's on?
            CALL YGGRAM (REED, F, BUFFER, IERROR)
            IF (IERROR .NE. 0) GOTO 950
            GRON = 0
            DO 20 I=1,3
               J = 2**(I-1)
               CALL ZGTBIT (N16, BUFFER(J+1), BITS)
               IF (BITS(16) .EQ. 1) GRON = ZOR(GRON,J)
 20         CONTINUE
C                                       Set up Blotch
            IF (GRON .NE. 0) THEN
               GBLOT = T
               IF (GRON .AND. N1) THEN
                     GRON = 1
                  ELSE
                     GRON = 2
               END IF
               CALL YGRAFE (REED, BLOTCH, STATUS, F, IERROR)
               IF (IERROR .NE. 0) GOTO 950
               CALL YGRAFE (RITE, GRON, STATUS, T, IERROR)
               IF (IERROR .NE. 0) GOTO 950
               DO 30 I=5,8
                  OUTSEL(I) = 3
 30            CONTINUE
            END IF
C                                       Set up IFM
            N = MAX0 (1, (OFMOUT+2) / (MAXINT+1))
            DO 40 I=1,OFMOUT+1
               BUFFER(I) = (I-1) / N
 40         CONTINUE
            CALL YIFM (RITE, N1, OFMOUT, F, F, BUFFER, IERROR)
            IF (IERROR .NE. 0) GOTO 950
C                                       Header items
            LUTPPB = 0
            BPLUT = 0
            OFMLEN = 0
            LUTPTR = 0
            OFMPTR = 0
            DATPTR = 512
C                                       B/W
            IF (BW) THEN
                  BITPIX = 8
                  CALL MAKHED (LUNOUT, FILNAM, DATEX, TIMEX, USERNO,
     *                         FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                         LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                         DATPTR, STRING)
                  DO 50 I=1,8
                     CONS(I) = OFMOUT
 50               CONTINUE
                  IC = 1
                  CALL FDBPLN (LUNOUT, CONS, OUTSEL, IC, NSCRPL, NX,
     *                         NY, INTSCA, BUFFER, IERROR)
                  IF (IERROR .NE. 0) GOTO 949
C                                       Color
               ELSE
                  BITPIX = 24
                  CALL MAKHED (LUNOUT, FILNAM, DATEX, TIMEX, USERNO,
     *                         FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                         LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                         DATPTR, STRING)
C                                       Open red, green, and blue files
                  OPEN (UNIT=LUNIC(1), FILE=FILNAM//'.BLU',
     *                  STATUS='SCRATCH', FORM='UNFORMATTED',
     *                  RECL=NX, RECORDTYPE='FIXED', ERR=962)
                  OPEN (UNIT=LUNIC(2), FILE=FILNAM//'.GRE',
     *                  STATUS='SCRATCH', FORM='UNFORMATTED',
     *                  RECL=NX, RECORDTYPE='FIXED', ERR=962)
                  OPEN (UNIT=LUNIC(3), FILE=FILNAM//'.RED',
     *                  STATUS='SCRATCH', FORM='UNFORMATTED',
     *                  RECL=NX, RECORDTYPE='FIXED', ERR=962)
C                                       Get the RGB planes
                  DO 70 IC=1,3
                     CALL FDBPLN (-LUNIC(IC), CONS, OUTSEL, IC, NSCRPL,
     *                            NX, NY, INTSCA, BUFFER, IERROR)
                     IF (IERROR .NE. 0) GOTO 949
                     DO 60 I=1,8
                        CONS(I) = OFMOUT
 60                  CONTINUE
 70               CONTINUE
C                                       Mix them and write them out
                  CALL COLMIX (LUNOUT, LUNIC, BUFFR1, BUFFER,
     *                         NX, NY, IERROR)
                  IF (IERROR .NE. 0) GOTO 960
C                                       Close red, green, and blue files
                  DO 80 I=1,3
                     CLOSE (UNIT=LUNIC(I))
 80               CONTINUE
            END IF
C
C                                       Case 2: get LUTs involved
C
         ELSE
C                                       Only if one plane on
            NSCRPL = 0
            J = 1
            DO 110 I=1,NGRAY
               IF (NPLNON .EQ. J) NSCRPL = I
               J = J * 2
 110        CONTINUE
            IF (NSCRPL .LE. 0) GOTO 970
C                                       Header items
            BITPIX = 8
            BPLUT = 16
            LUTPTR = 512
C                                       B/W
            IF (BW) THEN
                  LUTPPB = 1
                  OFMLEN = 0
                  OFMPTR = 0
                  DATPTR = LUTPTR+LUTPPB*BPLUT/8*BITPIX/8*256+OFMLEN*3
                  CALL MAKHED (LUNOUT, FILNAM, DATEX, TIMEX, USERNO,
     *                         FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                         LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                         DATPTR, STRING)
C                                       Get LUT
                  CALL YLUT (REED, NPLNON, N1, F, BUFFER, IERROR)
                  IF (IERROR .NE. 0) GOTO 950
                  SCL = 255.0 / LUTOUT
                  BUFFR2(1) = NINT(BUFFER(1) * SCL)
                  DO 120 I=2,255
                     BUFFR2(I) = NINT(BUFFER(INTSCA*I) * SCL)
 120              CONTINUE
                  BUFFR2(256) = NINT(BUFFER(MAXINT+1) * SCL)
                  CALL DICOUT (LUNOUT, BUFFR2, 512, IERROR)
                  IF (IERROR .NE. 0) GOTO 960
C                                       Color
               ELSE
                  LUTPPB = 3
                  OFMLEN = OFMINP + 1
                  OFMPTR = LUTPTR + 256 * LUTPPB * BPLUT/8 * BITPIX/8
                  DATPTR = LUTPTR+256*LUTPPB*BPLUT/8*BITPIX/8+OFMLEN*3
                  CALL MAKHED (LUNOUT, FILNAM, DATEX, TIMEX, USERNO,
     *                         FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                         LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                         DATPTR, STRING)
C                                       Get LUTs
                  DO 125 IC=3,1,-1
                     J = 2**(IC-1)
                     CALL YLUT (REED, NPLNON, J, F, BUFFER, IERROR)
                     IF (IERROR .NE. 0) GOTO 950
                     BUFFR2(1) = BUFFER(1)
                     DO 122 I=2,255
                        BUFFR2(I) = BUFFER(INTSCA*I)
 122                 CONTINUE
                     BUFFR2(256) = BUFFER(MAXINT+1)
                     CALL DICOUT (LUNOUT, BUFFR2, 512, IERROR)
                     IF (IERROR .NE. 0) GOTO 960
 125              CONTINUE
C                                       OFMs
                  SCL = 255.0 / OFMOUT
                  DO 150 IC=3,1,-1
                     J = 2**(IC-1)
                     CALL YOFM (REED, J, F, BUFFER, IERROR)
                     IF (IERROR .NE. 0) GOTO 950
                     DO 130 I=1,OFMLEN
                        I2 = NINT(BUFFER(I) * SCL)
                        BUFFR1(I) = I1
 130                 CONTINUE
                     CALL DICOUT (LUNOUT, BUFFR1, OFMLEN, IERROR)
                     IF (IERROR .NE. 0) GOTO 960
 150              CONTINUE
            END IF
C                                       Image
            CALL GETPLN (LUNOUT, NSCRPL, NX, NY, INTSCA, BUFFER,
     *                   IERROR)
            IF (IERROR .NE. 0) GOTO 949
C                                       End of it all
      END IF
      SUCCES = T
      GOTO 990
C                                       Error messages
 910  CONTINUE
      WRITE (MSGTXT,9100)
      CALL MSGWRT (8)
      GOTO 990
C
 920  CONTINUE
      WRITE (MSGTXT,9200) FILM1
      CALL MSGWRT (8)
      GOTO 990
C
 930  CONTINUE
      WRITE (MSGTXT,9300) FILM2
      CALL MSGWRT (8)
      GOTO 990
C
 940  CONTINUE
      WRITE (MSGTXT,9400) NGRAY
      CALL MSGWRT (8)
      GOTO 990
C
 949  CONTINUE
      IF (IERROR .LT. 0) THEN
            GOTO 960
         ELSE
            GOTO 950
      END IF
C
 950  CONTINUE
      WRITE (MSGTXT,9500)
      CALL MSGWRT (8)
      GOTO 990
C
 960  CONTINUE
      WRITE (MSGTXT,9600)
      CALL MSGWRT (8)
      GOTO 990
C
 961  CONTINUE
      WRITE (MSGTXT,9610)
      CALL MSGWRT (8)
      GOTO 990
C
 962  CONTINUE
      WRITE (MSGTXT,9620)
      CALL MSGWRT (8)
      GOTO 990
C
 970  CONTINUE
      WRITE (MSGTXT,9700)
      CALL MSGWRT (8)
      GOTO 990
C
C 980  CONTINUE
C      WRITE (MSGTXT,9800)
C      CALL MSGWRT (8)
C      WRITE (MSGTXT,9810)
C      CALL MSGWRT (8)
C      GOTO 990
C                                       Close and exit
 990  CONTINUE
      IF (SUCCES) THEN
            POTERR = 0
            CALL DICFLS (LUNOUT)
            CLOSE (UNIT=LUNOUT)
            I = SYSTEM('lpr -r -Pfw '//FILNAM)
         ELSE
            POTERR = 1
            IF (FILOPN) CLOSE (UNIT=LUNOUT, DISPOSE='DELETE')
      END IF
      IF (GBLOT) CALL YGRAFE (RITE, BLOTCH, STATUS, T, IERROR)
      IF (TVOPN) CALL TVCLOS (BUFFER, IERROR)
      COPIES = 1
      RETURN
C
C-----------------------------------------------------------------------
 9100 FORMAT ('TVDICO: Can''t open the TV')
 9200 FORMAT ('TVDICO: Illegal film size: ',A)
 9300 FORMAT ('TVDICO: Illegal film type: ',A)
 9400 FORMAT ('TVDICO: You are using TV channel',I2,'; I need it for ',
     *        'scratch')
 9500 FORMAT ('TVDICO: I have trouble communicating with the TV')
 9600 FORMAT ('TVDICO: I have trouble writing the image to disk')
 9610 FORMAT ('TVDICO: I can''t open the output image file')
 9620 FORMAT ('TVDICO: I can''t open my scratch files')
 9700 FORMAT ('TVDICO: You have more than one TV channel on')
C9800 FORMAT ('TVDICO: I can only deal with 8-bit images;')
C9810 FORMAT ('        ask your AIPS manager to re-configure the IVAS')
      END
      SUBROUTINE MAKHED (LUNOUT, FILNAM, DATEX, TIMEX, USERNO, FLMSIZ,
     *                   FLMTYP, NCOPY, BITPIX, NX, NY,
     *                   LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR, DATPTR,
     *                   STRING)
C-----------------------------------------------------------------------
C
C     This subroutine makes the Dicomed Image File header and writes
C     it out.
C
C-----------------------------------------------------------------------
C
      INTEGER LUNOUT, USERNO, FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY
      INTEGER LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR, DATPTR, IERROR
      INTEGER HOSTNM, I, J, K, I4
      BYTE A1(4), I1, BUFFER(512)
      CHARACTER*(*) FILNAM, DATEX, TIMEX, STRING
      CHARACTER*64 HEADER(8), SIZE(3)*4, TYPE(3)*6, HOST*8
C
      EQUIVALENCE (I4,A1(1)), (A1(4),I1)
C
      DATA SIZE, TYPE /'35mm','2x2 ','4x5', 'ColPos','ColNeg','B/W   '/
C
C-----------------------------------------------------------------------
C
      HEADER(1) = 'Filename:                               '//
     *            'Origin:                 '
      HEADER(2) = 'Date:               Time:               '//
     *            'Userno:                 '
      HEADER(3) = 'Filmsize:           Filmtype:           '//
     *            'Copies:                 '
      HEADER(4) = 'Bits/pixel:         Columns:            '//
     *            'Rows:                   '
      HEADER(5) = 'LUTs/pixelbyte:     Bits/LUTelement:    '//
     *            'OFMelements:            '
      HEADER(6) = 'LUTpointer:         OFMpointer:         '//
     *            'Datapointer:            '
      HEADER(7) = 'Usercomments:                           '//
     *            '                        '
      HEADER(8) = 'Recorded:                               '//
     *            '                        '
C
      I = HOSTNM (HOST)
      DO 10 I=1,8
         IF (ICHAR(HOST(I:I)) .EQ. 0) HOST(I:I) = ' '
 10   CONTINUE
      HEADER(1)(11:28) = FILNAM
      HEADER(1)(49:56) = HOST
      HEADER(2)(7:15) = DATEX
      HEADER(2)(27:34) = TIMEX
      WRITE (HEADER(2)(49:52), 1004) USERNO
      HEADER(3)(11:14) = SIZE(FLMSIZ)
      HEADER(3)(31:36) = TYPE(FLMTYP)
      WRITE (HEADER(3)(49:51), 1003) NCOPY
      WRITE (HEADER(4)(13:14), 1002) BITPIX
      WRITE (HEADER(4)(30:33), 1004) NX
      WRITE (HEADER(4)(47:50), 1004) NY
      WRITE (HEADER(5)(17:17), 1001) LUTPPB
      WRITE (HEADER(5)(38:39), 1002) BPLUT
      WRITE (HEADER(5)(54:58), 1004) OFMLEN
      WRITE (HEADER(6)(13:17), 1005) LUTPTR
      WRITE (HEADER(6)(33:37), 1005) OFMPTR
      WRITE (HEADER(6)(54:58), 1005) DATPTR
      HEADER(7)(15:64) = STRING
C
      K = 1
      DO 30 J=1,8
         DO 20 I=1,63
            I4 = ICHAR(HEADER(J)(I:I))
            BUFFER(K) = I1
            K = K + 1
 20      CONTINUE
         BUFFER(K) = 10
         K = K + 1
 30   CONTINUE
C
      CALL DICOUT (LUNOUT, BUFFER, 512, IERROR)
C
      RETURN
C-----------------------------------------------------------------------
 1001 FORMAT (I1)
 1002 FORMAT (I2)
 1003 FORMAT (I3)
 1004 FORMAT (I4)
 1005 FORMAT (I5)
      END
      SUBROUTINE FDBPLN (LUNIC, CONS, OUTSEL, IC, NSCRPL,
     *                            NX, NY, INTSCA, BUFFER, IERROR)
C-----------------------------------------------------------------------
C
C     This subroutine feeds back OFM <ic> to plane <nscrpl> (after
C     transferring  constants <cons> and output selection array
C     <outsel>), reads the plane out, and writes it to file <lunic>.
C     <nx> and <ny> give the size of the image.
C
C     Inputs:
C       lunic    I     LUN of output file
C       cons     I(8)  Constants array
C       outsel   I(8)  ALU outselect table
C       ic       I     Color number
C       nscrpl   I     Plane to use as scratch plane
C       nx       I     Number of pixels in X
C       ny       I     Number of pixels in Y
C       intsca   I     Scale factor for intensities
C       buffer   I(*)  Buffer
C     Output:
C       ierror   I     Return error code
C
C-----------------------------------------------------------------------
C
      INTEGER LUNIC, CONS(8), OUTSEL(8), IC, NSCRPL, NX, NY, IERROR
      INTEGER BUFFER(4096), INTSCA
      INTEGER N1, NPL, ICOL
      LOGICAL T, F, L1, L2
      CHARACTER*4 RITE
C
      DATA N1, T, F, RITE /1, .TRUE., .FALSE., 'WRIT'/
C
C-----------------------------------------------------------------------
C
      CALL YALUCT (RITE, T, N1, N1, CONS, OUTSEL, F, F, F, F, L1, L2,
     *             IERROR)
      IF (IERROR .EQ. 0) THEN
         NPL = 2**(NSCRPL-1)
         ICOL = MAX0 (1, MIN0 (3, IC))
         ICOL = 2**(ICOL-1)
         CALL YFDBCK (ICOL, NPL, 255, 0, F, F, F, F, F, IERROR)
         IF (IERROR .EQ. 0) THEN
            CALL GETPLN (LUNIC, NSCRPL, NX, NY, INTSCA, BUFFER,
     *                   IERROR)
         END IF
      END IF
      RETURN
      END
      SUBROUTINE GETPLN (LUN, NPL, NX, NY, INTSCA, BUFFER, IERROR)
C-----------------------------------------------------------------------
C
C     GETPLN reads TV channel <npl> (size <nx> by <ny>) and writes it
C     to file <lun>.
C
C-----------------------------------------------------------------------
C
      INTEGER LUN, NPL, NX, NY, INTSCA, BUFFER(NX), IERROR
      INTEGER I, J
      INTEGER*2 I2
      BYTE A1(2), I1, BYTBUF(4096)
      LOGICAL T, F
      CHARACTER*4 REED
C
      EQUIVALENCE (A1(1),I2), (A1(2),I1)
C
      DATA T, F, REED /.TRUE., .FALSE., 'READ'/
C
C-----------------------------------------------------------------------
C
      DO 50 J=NY,1,-1
         CALL YIMGIO (REED, NPL, 1, J, 0, NX, BUFFER, IERROR)
         IF (IERROR .NE. 0) GOTO 990
         DO 10 I=1,NX
            I2 = BUFFER(I) / INTSCA
            BYTBUF(I) = I1
 10      CONTINUE
         CALL DICOUT (LUN, BYTBUF, NX, IERROR)
         IF (IERROR .NE. 0) GOTO 990
 50   CONTINUE
C
 990  CONTINUE
      RETURN
      END
      SUBROUTINE DICOUT (LUN, BUF, NX, IERROR)
C-----------------------------------------------------------------------
C
C     Adds a single line (length <nx>) to the output buffer and writes
C     a record to <lun> whenever appropriate (i.e., there are at least
C     4096 bytes waiting).  If lun < 0, a direct write is done to the
C     file <abs(lun)>.
C
C-----------------------------------------------------------------------
C
      INTEGER LUN, NX, IERROR, I
      BYTE BUF(NX)
C
      INTEGER BUFPTR
      BYTE OUTBUF(8192), LINBUF(4096)
      COMMON /DICCOM/ BUFPTR, OUTBUF
      EQUIVALENCE (OUTBUF(1), LINBUF(1))
C
C-----------------------------------------------------------------------
C                                       Regular write
      IF (LUN .LT. 0) THEN
            WRITE (-LUN,ERR=900) BUF
         ELSE
C                                       Add to buffer
            DO 10 I=1,NX
               OUTBUF(BUFPTR+I) = BUF(I)
 10         CONTINUE
            BUFPTR = BUFPTR + NX
C                                       Flush one 4096-byte record
C                                       if there is enough
            IF (BUFPTR .GE. 4096) THEN
               WRITE (LUN,ERR=900) LINBUF
C                                       Shift the rest back, if any
               BUFPTR = BUFPTR - 4096
               IF (BUFPTR .GT. 0) THEN
                  DO 20 I=1,BUFPTR
                     OUTBUF(I) = OUTBUF(I+4096)
 20               CONTINUE
               END IF
            END IF
      END IF
      GOTO 990
C
 900  CONTINUE
      IERROR = -1
C
 990  CONTINUE
      RETURN
      END
      SUBROUTINE DICFLS (LUN)
C-----------------------------------------------------------------------
C
C     Flushes the buffer to <lun>.
C
C-----------------------------------------------------------------------
C
      INTEGER LUN, I
C
      INTEGER BUFPTR
      BYTE OUTBUF(8192), LINBUF(4096)
      COMMON /DICCOM/ BUFPTR, OUTBUF
      EQUIVALENCE (OUTBUF(1), LINBUF(1))
C
C-----------------------------------------------------------------------
C
      DO 10 I=BUFPTR+1,4096
         LINBUF(I) = 0
 10   CONTINUE
      WRITE (LUN) LINBUF
      RETURN
      END
      SUBROUTINE COLMIX (LUNOUT, LUNIC, BYTBUF, BUFFER, NX, NY, IERROR)
C-----------------------------------------------------------------------
C
C     colmix mixes the RGB bytes from the scratch files and writes the
C     result to <lunout>.
C     Inputs:
C       lunout     I     LUN of output file
C       lunic      I(3)  LUNs of input (RGB) files
C       bytbuf     B(*)  Input (read) buffer
C       buffer     B(*)  Output (write) buffer
C       nx         I     Number of pixels in X
C       ny         I     Number of pixels in Y
C     Output:
C       ierror     I     Return error code; 0 = OK, -1 = I/O error
C
C-----------------------------------------------------------------------
C
      INTEGER LUNOUT, LUNIC(3), NX, NY, IERROR
      INTEGER I, IC, J, K
      BYTE BYTBUF(NX), BUFFER(3*NX)
C
C-----------------------------------------------------------------------
C
      DO 10 IC=1,3
         REWIND LUNIC(IC)
 10   CONTINUE
C
      DO 50 J=1,NY
         DO 40 IC=3,1,-1
            READ (LUNIC(IC),ERR=900) BYTBUF
            K = 4 - IC
            DO 30 I=1,NX
               BUFFER(K) = BYTBUF(I)
               K = K + 3
 30         CONTINUE
 40      CONTINUE
         CALL DICOUT (LUNOUT, BUFFER, 3*NX, IERROR)
         IF (IERROR .NE. 0) GOTO 990
 50   CONTINUE
      GOTO 990
C
 900  CONTINUE
      IERROR = -1
      GOTO 990
C
 990  CONTINUE
      RETURN
      END
