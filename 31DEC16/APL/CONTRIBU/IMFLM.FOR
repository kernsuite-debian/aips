      PROGRAM IMFLM
C-----------------------------------------------------------------------
C! For queueing disk images to Dicomed
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
C                !           I M F L M           !
C                !                               !
C                ! Version 1.0 - 20 February 1990!
C                !                               !
C                !          Arnold Rots          !
C                !                               !
C                ---------------------------------
C
C
C     IMFLM generates an image from a disk file with the TV transfer function
C     in a Dicomed Image File and queues it.  For cubes it will make a multi-
C     panel composite.
C
C     Version 1.0: New task  -  900220 ahr
C
C     INPUT ADVERBS :
C   USERID     -32000.0      32000.0    User ID.  0 => current user,
C                                         32000 => any user.
C   INNAME                              Image name(name).
C   INCLASS                             Image name(class).
C   INSEQ           0.0       9999.0    Image name(seq. #). 0=>high
C   INDISK          0.0          9.0    Disk drive #. 0=>any
C   TBLC            0.0       4096.0    Bottom Left Corner of image
C                                         0=>entire image
C   TTRC            0.0       4096.0    Top Right Corner of image
C                                         0=>entire image
C   TXINC        -100.0        256.0    Load every TXINC'th column
C   TYINC        -100.0        256.0    Load every TYINC'th row
C   TZINC        -100.0        256.0    Load every TZINC'th plane
C   PIXRANGE                            Min,Max  of image intensity
C                                         0=>entire range
C   FUNCTYPE                            Image intensity transfer func
C                                         'LN' Linear.  unknown=>'LN'
C                                         'LG' Logarithmic
C                                         'NE' Negative linear
C                                         'NG' Negative logarithmic
C   COPIES          0.0        200.0    Number of copies requested
C   STRA1                               Specifies film size and type
C   OUTPRINT                            User comment
C
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, PS*40
      REAL      CHAN(14), COPIES, STRA1, OUTPRT(12), PARM(42)
      INTEGER   NSUB, NPARM, NRET, ERROR, POTERR
      INTEGER   BUFF(256)
      LOGICAL   QUICK
      EQUIVALENCE (CHAN(1), COPIES), (CHAN(2), STRA1),
     *            (CHAN(3),OUTPRT(1)), (CHAN(1), PARM(29))
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NPARM /42/, PRGNAM /'IMFLM '/
C-----------------------------------------------------------------------
C                                       I/O handling
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE., BUFF)
      CALL VHDRIN
C                                       get inputs
      CALL GTPARM (PRGNAM, NPARM, QUICK, PARM, BUFF, ERROR)
      IF (ERROR .NE. 0) GO TO 900
      CALL YTVCIN
C                                       open TV
      CALL IMDICO (POTERR, COPIES, STRA1, OUTPRT, PARM, QUICK)
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
      SUBROUTINE IMDICO (POTERR, COPIES, STRA1, OUTPRT, PARM, QUICK)
C
C           +-----------------------------------+
C           |                                   |
C           |            i m d i c o            |
C           |                                   |
C           |  Version 1.0  -  20 February 1990 |
C           |                                   |
C           |            Arnold Rots            |
C           |                                   |
C           +-----------------------------------+
C
C     IMDICO is the routine that queues the disk image to the film writer.
C
C     Version 1.0: New program  -  900220 ahr
C
C     Input params:
C       copies   I   Number of copies to be made
C       stra1   C*4  String containing film type and size code
C       outprt  C*48 User comment string
C       parm    R*28 Image info
C       quick    L   Whether or not to release AIPS
C     Output params:
C       poterr   I   Exit error code
C
C-----------------------------------------------------------------------
C
      REAL COPIES, STRA1, OUTPRT(12), PARM(28), EPS, BLC(7), TRC(7), SCL
      INTEGER N1, BUFFER(4096), LUNOUT, IVOL, USID, SEQNO, INC(3), MAGIC
      INTEGER NX, NY, IERROR, I, J, K, NPLNON, NPON(12), FLMSIZ, FLMTYP
      INTEGER NCOPY, NSCRPL, NSPLBT, N16, BITS(16), DLUN, DIND, CNO,
     *        STATUS, IZP, ZINC, NNX, NNY, NNZ, LX, LY, MX, MY, ITVC(4)
      INTEGER N, BITPIX, LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR, DATPTR
      INTEGER USERNO, IC, MONOFF(12), SYSTEM, POTERR, INTSCA, TYPE
      INTEGER IWIN(4)
      INTEGER*2 I2, BUFFR2(256)
      BYTE I1, A1(2), BUFFR1(4096)
      BYTE FRBUFF(16777216)
      CHARACTER*4 REED, RITE, FILNAM*18, FILM1, FILM2*6, DATEX*9
      CHARACTER*8 TIMEX, STRING*50, TSTR*2, NAME*12, CLASS*6, PTYPE*2
      LOGICAL QUICK, F, T, MAOPEN, FILOPN, TVOPN, SUCCES, BW, CUBE
      LOGICAL RELEAS
C
C     INCLUDE 'INCS:DAPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      INTEGER BUFPTR
      BYTE OUTBUF(8192), LINBUF(4096)
      COMMON /DICCOM/ BUFPTR, OUTBUF
      EQUIVALENCE (OUTBUF(1), LINBUF(1))
C
      EQUIVALENCE (I2,A1(1)), (I1, A1(2))
      EQUIVALENCE (BUFFER(1), BUFFR2(1))
C
      DATA N1, N16, F, T, REED, RITE, EPS, DLUN, MAGIC
     *     /1, 16, .FALSE., .TRUE., 'READ', 'WRIT', 0.1, 16, 32000/
      DATA MONOFF
     *     /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
C
C-----------------------------------------------------------------------
C                                       Initialize
      BUFPTR = 0
      IERROR = 0
      MAOPEN = F
      FILOPN = F
      TVOPN = F
      SUCCES = F
      RELEAS = F
      LUNOUT = 101
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
      INTSCA = MAX0(1, (MAXINT+2)/256)
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
            NX = 1536
            NY = 1024
         ELSE IF ((FILM1 .EQ. '2x2') .OR. (FILM1 .EQ. '2X2')) THEN
            FLMSIZ = 2
            NX = 2048
            NY = 2048
         ELSE IF ((FILM1 .EQ. '4x5') .OR. (FILM1 .EQ. '4X5')) THEN
            FLMSIZ = 3
            NX = 4096
            NY = 4096
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
C                                       Open map and determine size
C
      MAXXTV(1) = NX
      MAXXTV(2) = NY
      I = 1
      IVOL = PARM(8) + EPS
      USID = ABS(PARM(1)) + EPS
      SEQNO = PARM(7) + EPS
      IF (USID.EQ.0) USID = NLUSER
      IF (USID.EQ.MAGIC) USID = 0
      CALL H2CHR (12, 1, PARM(2), NAME)
      CALL H2CHR (6, 1, PARM(5), CLASS)
      INC(1) = NINT(PARM(23))
      INC(2) = NINT(PARM(24))
      INC(3) = NINT(PARM(25))
      PTYPE = 'MA'
      CALL MAPOPN (REED, IVOL, NAME, CLASS, SEQNO, PTYPE, USID,
     *             DLUN, DIND, CNO, CATBLK, BUFFER, IERROR)
      POTERR = 33
      IF (IERROR.GT.1) GO TO 975
      MAOPEN = T
      CALL RCOPY (7, PARM(9), BLC)
      CALL RCOPY (7, PARM(16), TRC)
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERROR)
      IF (IERROR .NE. 0) GOTO 980
      IWIN(1) = BLC(1)
      IWIN(2) = BLC(2)
      IWIN(3) = TRC(1)
      IWIN(4) = TRC(2)
C                                       Image cat fill in some
      CALL RNGSET (PARM(26), CATR(KRDMX), CATR(KRDMN), CATR(IRRAN))
      CATBLK(IIVOL) = IVOL
      CATBLK(IICNO) = CNO
      CATH(IITRA) = PARM(28)
C                                       Decide 3-D vs 2-D
      ZINC = MAX0 (1, INC(3))
      CUBE = F
      IF (CATBLK(KIDIM) .GE. 3) THEN
         DO 15 I=3,CATBLK(KIDIM)
            IF ((((TRC(I)-BLC(I))/ZINC) .GT. 1.0) .AND. (.NOT. CUBE))
     *          THEN
               CUBE = T
               IZP = I
            END IF
            CATBLK(IIDEP+I-3) = NINT(BLC(I))
 15      CONTINUE
      END IF
C                                       Cube
      IF (CUBE) THEN
            DO 20 I=1,3
               INC(I) = MAX0(1,INC(I))
 20         CONTINUE
            NNX = (TRC(1) - BLC(1)) / INC(1) + 1.01
            NNY = (TRC(2) - BLC(2)) / INC(2) + 1.01
            NNZ = (TRC(IZP) - BLC(IZP)) / INC(3) + 1.01
            LX = (NX - 1) / (NNX + 1)
            LY = (NY - 1) / (NNY + 1)
            IF ((LX * LY) .LT. NNZ) GOTO 985
            MX = NINT(SQRT(NNZ*(FLOAT(NX*(NNY+1))/FLOAT(NY*(NNX+1)))))
            MX = MIN0(MX, LX)
            MY = (NNZ-1) / MX + 1
            MY = MIN0(MY, LY)
            MX = (NNZ-1) / MY + 1
            NX = (NNX + 1) * MX + 1
            NY = (NNY + 1) * MY + 1
C                                       2-D image
         ELSE
            ITVC(1) = 1
            ITVC(2) = 1
            POTERR = 49
            TYPE = -1
            I = 1
            CALL TVWIND (TYPE, INC, BLC, TRC, I, ITVC, IWIN, IERROR)
            IF (IERROR .NE. 0) GOTO 980
            NX = ITVC(3) - ITVC(1) + 1
            NY = ITVC(4) - ITVC(2) + 1
      END IF
C
C                                       Get TV items
C
C                                       Only if one plane on
      NSCRPL = 0
      J = 1
      DO 110 I=1,NGRAY
         IF (NPLNON .EQ. J) NSCRPL = I
         J = J * 2
 110  CONTINUE
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
     *                   FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                   LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                   DATPTR, STRING)
C                                       Get LUT
            CALL YLUT (REED, NPLNON, N1, F, BUFFER, IERROR)
            IF (IERROR .NE. 0) GOTO 950
            SCL = 255.0 / LUTOUT
            BUFFR2(1) = NINT(BUFFER(1) * SCL)
            DO 120 I=2,255
               BUFFR2(I) = NINT(BUFFER(INTSCA*I) * SCL)
 120        CONTINUE
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
     *                   FLMSIZ, FLMTYP, NCOPY, BITPIX, NX, NY,
     *                   LUTPPB, BPLUT, OFMLEN, LUTPTR, OFMPTR,
     *                   DATPTR, STRING)
C                                       Get LUTs
            DO 125 IC=3,1,-1
               J = 2**(IC-1)
               CALL YLUT (REED, NPLNON, J, F, BUFFER, IERROR)
               IF (IERROR .NE. 0) GOTO 950
               BUFFR2(1) = BUFFER(1)
               DO 122 I=2,255
                  BUFFR2(I) = BUFFER(INTSCA*I)
 122           CONTINUE
               BUFFR2(256) = BUFFER(MAXINT+1)
               CALL DICOUT (LUNOUT, BUFFR2, 512, IERROR)
               IF (IERROR .NE. 0) GOTO 960
 125        CONTINUE
C                                       OFMs
            SCL = 255.0 / OFMOUT
            DO 150 IC=3,1,-1
               J = 2**(IC-1)
               CALL YOFM (REED, J, F, BUFFER, IERROR)
               IF (IERROR .NE. 0) GOTO 950
               DO 130 I=1,OFMLEN
                  I2 = NINT(BUFFER(I) * SCL)
                  BUFFR1(I) = I1
 130           CONTINUE
               CALL DICOUT (LUNOUT, BUFFR1, OFMLEN, IERROR)
               IF (IERROR .NE. 0) GOTO 960
 150        CONTINUE
      END IF
C                                       Image
C                                       Release TV & AIPS
      CALL TVCLOS (BUFFER, IERROR)
      TVOPN = F
      IF (QUICK) THEN
         I = 0
         CALL RELPOP (I, BUFFER, IERROR)
         IF (IERROR .EQ. 0) RELEAS = T
      END IF
C                                       Fake the TV common for TVLOD
      MAXINT = 255
C                                       Now really get the image
      IF (CUBE) THEN
            CALL IM3LOD (DLUN, DIND, NX, NY, INC, IWIN, 4096, BUFFER,
     *                   FRBUFF, NNX, NNY, NNZ, MX, MY, IZP, IERROR)
            IF (IERROR .NE. 0) GOTO 940
         ELSE
            NNX = NX
            NNY = NY
            CALL IM2LOD (DLUN, DIND, NX, NY, INC, IWIN, 4096, BUFFER,
     *                   FRBUFF, NNX, NNY, 1, 1, IERROR)
            IF (IERROR .NE. 0) GOTO 940
      END IF
      CALL WRTPLN (LUNOUT, FRBUFF, NX, NY, IERROR)
      IF (IERROR .NE. 0) GOTO 949
C                                       End of it all
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
      WRITE (MSGTXT,9400)
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
 970  CONTINUE
      WRITE (MSGTXT,9700)
      CALL MSGWRT (8)
      GOTO 990
C
 975  CONTINUE
      WRITE (MSGTXT,9750)
      CALL MSGWRT (8)
      GOTO 990
C
 980  CONTINUE
      WRITE (MSGTXT,9800)
      CALL MSGWRT (8)
      GOTO 990
C
 985  CONTINUE
      WRITE (MSGTXT,9850)
      CALL MSGWRT (8)
      GOTO 990
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
      QUICK = RELEAS
      IF (MAOPEN) CALL MAPCLS (REED, IVOL, CNO, DLUN, DIND, CATBLK,
     *                          F, BUFFER, IERROR)
      IF (TVOPN) CALL TVCLOS (BUFFER, IERROR)
      COPIES = 1
      RETURN
C
C-----------------------------------------------------------------------
 9100 FORMAT ('IMDICO: Can''t open the TV')
 9200 FORMAT ('IMDICO: Illegal film size: ',A)
 9300 FORMAT ('IMDICO: Illegal film type: ',A)
 9400 FORMAT ('IMDICO: I have trouble composing your frame')
 9500 FORMAT ('IMDICO: I have trouble communicating with the TV')
 9600 FORMAT ('IMDICO: I have trouble writing the image to disk')
 9610 FORMAT ('IMDICO: I can''t open the output image file')
 9700 FORMAT ('IMDICO: You have more than one TV channel on')
 9750 FORMAT ('IMDICO: I can''t open your map file')
 9800 FORMAT ('IMDICO: There is something wrong with the BLC/TRC')
 9850 FORMAT ('IMDICO: There are not enough film pixels for all this')
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
      SUBROUTINE WRTPLN (LUN, FRBUFF, NX, NY, IERROR)
C-----------------------------------------------------------------------
C
C     wrtpln writes the frame buffer frbuff (size <nx> by <ny>)
C     to file <lun>.
C
C-----------------------------------------------------------------------
C
      INTEGER LUN, NX, NY, IERROR
      INTEGER J
      BYTE FRBUFF(NX,NY)
C
C-----------------------------------------------------------------------
C
      DO 50 J=NY,1,-1
         CALL DICOUT (LUN, FRBUFF(1,J), NX, IERROR)
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
C     4096 bytes waiting).  For lun<0 it is just a direct write to <-lun>.
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
C     Flushes the output buffer to <lun>.
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
      SUBROUTINE IM2LOD (LUN, IND, NX, NY, PXINC, WIN, BUFSZ, BUFF,
     *   FRBUFF, NNX, NNY, I0, J0, IERR)
C-----------------------------------------------------------------------
C! load image to a TV memory from open MA file
C# TV-util
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
C   Subroutine to load a map from an already opened map file to an in-
C   memory frame buffer. It assumes that the windows are all computed.
C   One can think of the frame buffer as a TV device.
C   Inputs:
C      LUN      I      Logical unit # of map file
C      IND      I      FTAB pointer for map file
C      NX       I      Size of FRBUFF in X
C      NY       I      Size of FRBUFF in Y
C      PXINC    I(2)   Increment in x,y between included pixels
C                      Negative values indicate interpolation
C      WIN      I(4)   Map window: BLC x,y TRC x,y
C      BUFSZ    I      Buffer size in bytes
C      BUFF     R(*)   Read buffer
C      NNX      I      Number of pixels to fill in X
C      NNY      I      Number of pixels to fill in Y
C      I0       I      Lower LH corner of inage in FRBUFF
C      J0       I          "          "
C   Outputs
C      FRBUFF   B(NX,NY) Frame buffer
C      IERR     I      Error code: 0 => ok
C                                  1 => input errors
C                                  2 => MINIT errors
C                                  3 => MDISK errors
C   Commons: /MAPHDR/ CATBLK  image header
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, PXINC(2), IMAWIN(4), WIN(4), BUFSZ, IERR
      INTEGER   NNX, NNY, I0, J0, II0, NX, NY
      REAL      BUFF(*)
      BYTE      FRBUFF(NX, NY)
C
      CHARACTER TRANFN*2
      INTEGER   JERR, ININD, DELBLK, I, IYTV, MPIX, NPIX, IM1, IM2,
     *   HORIZ, IX, PLINC(2), NXINT, NYINT, LPIX, IBUFF(4096),
     *   JBUFF(4096), IY, TBUFF(4096), II
      INTEGER*2 I2
      BYTE AI(2), I1
      EQUIVALENCE (AI(1), I2), (AI(2), I1)
      REAL      EPS, X, Y
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA EPS, HORIZ /0.01, 0/
C-----------------------------------------------------------------------
      IERR = 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), DELBLK,
     *   JERR)
      IF (JERR.NE.0) GO TO 999
      DELBLK = DELBLK + 1
      IMAWIN(1) = I0
      IMAWIN(2) = J0
      IMAWIN(3) = I0 + NNX - 1
      IMAWIN(4) = J0 + NNY - 1
      II0 = I0 -1
C                                       fix up img cat block
      CALL COPY (4, WIN, CATBLK(IIWIN))
      CALL COPY (4, IMAWIN, CATBLK(IICOR))
      CALL H2CHR (2, 1, CATH(IITRA), TRANFN)
C                                       start map read
      PLINC(1) = MAX (1, PXINC(1))
      PLINC(2) = MAX (1, PXINC(2))
      NXINT = MAX (1, -PXINC(1))
      NYINT = MAX (1, -PXINC(2))
      NPIX = 1 + WIN(3) - WIN(1)
      MPIX = 1 + (IMAWIN(3) - IMAWIN(1)) / NXINT
      LPIX = 1 + IMAWIN(3) - IMAWIN(1)
      CALL MINIT ('READ', LUN, IND, CATBLK(KINAX), CATBLK(KINAX+1), WIN,
     *   BUFF, BUFSZ, DELBLK, JERR)
      IF (JERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) JERR
         GO TO 980
C                                       set up TV writing
 20   IM1 = WIN(2)
      IM2 = WIN(4)
      IERR = 3
      IYTV = IMAWIN(2)-1
C                                       read 1st row
      CALL MDISK ('READ', LUN, IND, BUFF, ININD, JERR)
      IF (JERR.EQ.0) GO TO 25
         WRITE (MSGTXT,1020) JERR
         GO TO 980
 25   CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX, PLINC,
     *   BUFF(ININD), TBUFF)
      CALL LINTER (MPIX, NXINT, TBUFF, IBUFF)
      DO 70 IY = IM1,IM2
C                                       End: write row only
         IF (IY.LT.IM2) GO TO 30
            IYTV = IYTV + 1
            DO 27 II=1,NNX
               I2 = IBUFF(II)
               FRBUFF(II+II0,IYTV) = I1
 27         CONTINUE
            GO TO 70
 30      CALL MDISK ('READ', LUN, IND, BUFF, ININD, JERR)
         IF (JERR.EQ.0) GO TO 40
            WRITE (MSGTXT,1020) JERR
            GO TO 980
 40      IF (MOD(IY-IM1,PLINC(2)).NE.0) GO TO 70
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX, PLINC,
     *         BUFF(ININD), TBUFF)
            CALL LINTER (MPIX, NXINT, TBUFF, JBUFF)
            IYTV = IYTV + 1
            DO 47 II=1,NNX
               I2 = IBUFF(II)
               FRBUFF(II+II0,IYTV) = I1
 47         CONTINUE
C                                       interpolate rows
            IF (NYINT.LE.1) GO TO 65
               X = 1.0 / NYINT
               DO 60 I = 2,NYINT
                  Y = (I-1) * X
                  DO 55 IX = 1,LPIX
                     TBUFF(IX) = IBUFF(IX) + Y * (JBUFF(IX) - IBUFF(IX))
     *                  + 0.4999
 55                  CONTINUE
                  IYTV = IYTV + 1
                  DO 57 II=1,NNX
                     I2 = TBUFF(II)
                     FRBUFF(II+II0,IYTV) = I1
 57               CONTINUE
 60            CONTINUE
 65         CALL COPY (LPIX, JBUFF, IBUFF)
 70      CONTINUE
      IERR = 0
      GO TO 999
C                                       write error message
 980  CALL MSGWRT (7)
C
 999   RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('IM2LOD: MINIT DISK I/O ERROR =',I6)
 1020 FORMAT ('IM2LOD: MDISK DISK I/O ERROR =',I6)
      END
      SUBROUTINE IM3LOD (LUN, IND, NX, NY, INC, IWIN, BUFSIZ, BUFFER,
     *                   FRBUFF, NNX, NNY, NNZ, MX, MY, IZP, IERROR)
C-----------------------------------------------------------------------
C
C     IM3LOD writes cube panels, using IM2LOD.  It puts the individual
C     planes in a mosaic with borders and uses IM2LOD to paint each pane.
C     The call parameters are pretty much the same as IM2LOD's:
C   Inputs:
C      LUN      I      Logical unit # of map file
C      IND      I      FTAB pointer for map file
C      NX       I      Size of FRBUFF in X
C      NY       I      Size of FRBUFF in Y
C      INC      I(3)   Increment in x,y,z between included pixels
C                      Negative values indicate interpolation
C      IWIN     I(4)   Map window: BLC x,y TRC x,y
C      BUFSIZ   I      Buffer size in bytes
C      BUFFER   R(*)   Buffer
C      NNX      I      Number of pixels to fill in X
C      NNY      I      Number of pixels to fill in Y
C      NNZ      I      Number of panes
C      MX       I      Number of panes in X
C      MY       I      Number of panes in Y
C      IZP      I      Dimension number of third axis
C   Outputs
C      FRBUFF   B(NX,NY) Frame buffer
C      IERROR   I      Error code: 0 => ok
C                                  1 => input errors
C                                  2 => MINIT errors
C                                  3 => MDISK errors
C
C-----------------------------------------------------------------------
C
      INTEGER LUN, IND, NX, NY, INC(3), IWIN(4), BUFSIZ, BUFFER(BUFSIZ)
      INTEGER NNX, NNY, NNZ, MX, MY, IZP, IERROR
      INTEGER I, J, K, L, M, I0, J0
      BYTE FRBUFF(NX,NY)
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C
C-----------------------------------------------------------------------
C
C                                       Prepare the frame
      DO 20 J=1,NY-1
         FRBUFF(1,J) = -1
         DO 10 I=2,NX
            FRBUFF(I,J) = 0
 10      CONTINUE
 20   CONTINUE
      DO 30 I=1,NX
         FRBUFF(I,NY) = -1
 30   CONTINUE
C                                       Loop over planes
      CATBLK(IIDEP+IZP-3) = CATBLK(IIDEP+IZP-3) - INC(3)
      K = 0
      DO 110 M=1,MY
         J0 = NY - M * (NNY+1) + 1
         DO 100 L=1,MX
            K = K + 1
            I0 = 2 + (L - 1) * (NNX+1)
            IF (K .LE. NNZ) THEN
               CATBLK(IIDEP+IZP-3) = CATBLK(IIDEP+IZP-3) + INC(3)
               CALL IM2LOD (LUN, IND, NX, NY, INC, IWIN, BUFSIZ, BUFFER,
     *                      FRBUFF, NNX, NNY, I0, J0, IERROR)
               IF (IERROR .NE. 0) GOTO 900
               J = J0 - 1
               DO 80 I=I0,I0+NNX
                  FRBUFF(I,J) = -1
 80            CONTINUE
               I = I0 + NNX
               DO 90 J=J0,J0+NNY
                  FRBUFF(I,J) = -1
 90            CONTINUE
            END IF
 100     CONTINUE
 110  CONTINUE
C
 900  CONTINUE
      RETURN
      END
