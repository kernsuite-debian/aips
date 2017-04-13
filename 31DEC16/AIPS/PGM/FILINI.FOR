      PROGRAM FILINI
C-----------------------------------------------------------------------
C! FILINI creates and initializes AIPS files.
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998
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
C   FILINI is a service program to create & initialize AIPS files.
C-----------------------------------------------------------------------
      CHARACTER MPASS*12, PHNAME*48, FTYP(11)*2, ITYP*2, MSGBUF*80,
     *   OPCODE(4)*4, OP*4, PRGNAM*6, LPASS*12
      HOLLERITH APASS(3)
      INTEGER   ISEQ, IVER, IERR, LUN, FIND, IQ1, IQ2, I, J1, J2, IR,
     *   INPOPS, BUFFER(256), NL, ILUSER, LTVDEV, TTYLUN, TTYIND, IVOL,
     *   IC, ICMAX, IMAX, INPOP1, INPOP2, IOP, IQ, IVOL1, IVOL2, J,
     *   NOP, IUS1, IUS2, I4T, ISIZE, IRNO, LSIZE, TTY(2), IARR(5),
     *   LIST(6), NREC
      REAL      RBUFFR(256)
      DOUBLE PRECISION DOUT
      LOGICAL   F, T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PKSZ.INC'
      EQUIVALENCE (BUFFER, RBUFFR)
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA MPASS /'AMANAGER    '/
      DATA OPCODE /'QUIT','CREA','DEST','INIT'/
      DATA F, T /.FALSE.,.TRUE./
      DATA LUN, TTYLUN /15,5/
      DATA LIST /2, -1, -1, -1, -1, 0/
      DATA NOP, FTYP /11,
     *   'BQ','BA','MS','IC','CA','TD','AC','ID','GR','PW','TC'/
      DATA PRGNAM /'FILINI'/
C-----------------------------------------------------------------------
C                                        Init itself
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      ILUSER = 0
      LTVDEV = NTVDEV
      CALL YTVCIN
C                                       Is this job ok?
C                                       Does password file exist
      CALL ZPHFIL (FTYP(10), 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
C                                       Ask for password
      IF (IERR.EQ.0) THEN
         CALL PASWRD (BUFFER, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE IF (IERR.NE.1) THEN
         WRITE (MSGTXT,1005) IERR
         CALL MSGWRT (8)
         END IF
C                                        get basic op
 10   WRITE (MSGBUF,1010)
      CALL INQSTR (TTY, MSGBUF, 4, OP, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL CHLTOU (4, OP)
      IF (OP.EQ.OPCODE(1)) GO TO 995
      IF (OP.EQ.OPCODE(4)) GO TO 50
      IF (OP.EQ.OPCODE(3)) GO TO 30
      IF (OP.NE.OPCODE(2)) GO TO 10
C                                        CREATE
 20   WRITE (MSGBUF,1020) ILUSER
      CALL INQGEN (TTYLUN, MSGBUF, LIST, IARR, DOUT, ITYP, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 20
      IVOL = IARR(1)
      ISEQ = IARR(2)
      IVER = IARR(3)
      IR = IARR(4)
      IF (IR.GT.0) ILUSER = IR
 25   WRITE (MSGBUF,1022) 0
      ISIZE = 3
      ISIZE = 256 / ISIZE
      ISIZE = (USELIM - 1) / ISIZE + 1
C                                       memory file
      IF (ITYP.EQ.'ME') THEN
         ISIZE = KBLOCK + LBLOCK
C                                       Not PW file
      ELSE IF (ITYP.NE.FTYP(10)) THEN
         CALL INQINT (TTY, MSGBUF, 1, ISIZE, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (IERR.LT.0) GO TO 25
         IF (ISIZE.LE.0) ISIZE = 2
         END IF
      NLUSER = ILUSER
      CALL ZPHFIL (ITYP, IVOL, ISEQ, IVER, PHNAME, IERR)
      NLUSER = 1
      CALL ZEXIST (1, PHNAME, LSIZE, IERR)
      IF (IERR.EQ.0) THEN
         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 910
         IF (LSIZE.LT.ISIZE) THEN
            MSGTXT = 'Expanding the file'
            CALL MSGWRT (2)
            NREC = (ISIZE - LSIZE)
            CALL ZEXPND (LUN, IVOL, PHNAME, NREC, IERR)
         ELSE IF (LSIZE.GT.ISIZE) THEN
            MSGTXT = 'Compressing the file'
            CALL MSGWRT (2)
            CALL ZCMPRS (IVOL, PHNAME, LUN, ISIZE, IERR)
         ELSE
            MSGTXT = 'File already exists of correct size'
            CALL MSGWRT (2)
            END IF
         CALL ZCLOSE (LUN, FIND, I)
      ELSE IF (IERR.EQ.1) THEN
         CALL ZCREAT (IVOL, PHNAME, ISIZE, F, LSIZE, IERR)
         END IF
      IF (IERR.NE.0) GO TO 900
      GO TO 10
C                                        DESTROY
 30   WRITE (MSGBUF,1020) ILUSER
      CALL INQGEN (TTYLUN, MSGBUF, LIST, IARR, DOUT, ITYP, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 30
      IVOL = IARR(1)
      ISEQ = IARR(2)
      IVER = IARR(3)
      IR = IARR(4)
      IF (IR.GT.0) ILUSER = IR
      NLUSER = ILUSER
      CALL ZPHFIL (ITYP, IVOL, ISEQ, IVER, PHNAME, IERR)
      NLUSER = 1
      CALL ZDESTR (IVOL, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 10
C-----------------------------------------------------------------------
C                                        INIT
 50   WRITE (MSGBUF,1050)
      CALL INQSTR (TTY, MSGBUF, 2, ITYP, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL CHLTOU (2, ITYP)
      DO 55 IOP = 1,NOP
         IF (ITYP.EQ.FTYP(IOP)) GO TO 60
 55      CONTINUE
      GO TO 10
C
 60   GO TO (100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600),
     *   IOP
      GO TO 10
C-----------------------------------------------------------------------
C                                        batch queuing
 100  WRITE (MSGBUF,1100)
      CALL INQINT (TTY, MSGBUF, 3, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 100
      IQ = IARR(1)
      J1 = IARR(2)
      J2 = IARR(3)
      CALL ZPHFIL (ITYP, 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      IF ((IQ.GT.0) .AND. (IQ.LE.NBATQS)) THEN
         IQ1 = IQ
         IQ2 = IQ
      ELSE
         IQ1 = 1
         IQ2 = NBATQS
         END IF
      J1 = MAX (1,J1)
      J2 = MIN (64,J2)
      IF (J2.LT.J1) THEN
         J1 = 1
         J2 = 64
         END IF
      J2 = 4 * (J2 - J1 + 1)
      J1 = 4*J1 - 3
      DO 130 IRNO = IQ1,IQ2
         CALL ZFIO ('READ', LUN, FIND, IRNO, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 920
         CALL FILL (J2, 0, BUFFER(J1))
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 930
 130     CONTINUE
      GO TO 950
C-----------------------------------------------------------------------
C                                        batch work file
 150  WRITE (MSGBUF,1150)
      CALL INQINT (TTY, MSGBUF, 2, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 150
      IQ = IARR(1)
      INPOPS = IARR(2)
      IF ((IQ.GE.1) .AND. (IQ.LE.NBATQS)) THEN
         IQ1 = IQ
         IQ2 = IQ
      ELSE
         IQ1 = 1
         IQ2 = NBATQS
         END IF
C                                       note BATER runs NPOPS =
C                                       NINTRN + 1
      IF ((INPOPS.GE.1) .AND. (INPOPS.LE.NINTRN+1)) THEN
         INPOP1 = INPOPS
         INPOP2 = INPOPS
      ELSE
         INPOP1 = 1
         INPOP2 = NINTRN + 1
         END IF
      DO 175 INPOPS = INPOP1,INPOP2
         DO 170 IQ = IQ1,IQ2
            CALL ZPHFIL (ITYP, 1, IQ, INPOPS, PHNAME, IERR)
            CALL ZEXIST (1, PHNAME, ISIZE, IERR)
            IF (IERR.NE.0) GO TO 940
            IR = ISIZE
            CALL FILL (256, 0, BUFFER)
            BUFFER(2) = 2
            BUFFER(3) = 1
            BUFFER(4) = IR
            CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
            IF (IERR.NE.0) GO TO 910
            CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 930
            CALL ZCLOSE (LUN, FIND, IERR)
 170        CONTINUE
 175     CONTINUE
      GO TO 900
C-----------------------------------------------------------------------
C                                        message files
 200  WRITE (MSGBUF,1502) ILUSER
      CALL INQINT (TTY, MSGBUF, 1, I, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 200
      IF (I.GT.0) ILUSER = I
      IVOL = 1
      NLUSER = ILUSER
      CALL ZPHFIL (ITYP, IVOL, ILUSER, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
      NLUSER = 1
      IF (IERR.NE.0) GO TO 940
      CALL FILL (256, 0, BUFFER)
      BUFFER(2) = ISIZE
      CALL ZOPEN (LUN, FIND, IVOL, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 930
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 900
C-----------------------------------------------------------------------
C                                        image catalog
 250  IF (NTVDEV.LE.0) GO TO 270
         IC = NTVDEV
         WRITE (MSGTXT, 1230) IC
         CALL MSGWRT (2)
         MSGTXT = 'Use SETTVP to change TV parameters'
         CALL MSGWRT (2)
         DO 265 NTVDEV = 1,IC
 252        WRITE (MSGBUF,1250) NTVDEV
            CALL INQINT (TTY, MSGBUF, 1, J1, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.LT.0) GO TO 252
            IF (J1.EQ.-2) GO TO 269
            IF (J1.LT.0) GO TO 265
               CALL YTVCIN
               MSGTXT =  'Use TVINIT when running AIPS TVs'
               CALL MSGWRT (2)
 265        CONTINUE
 269     NTVDEV = IC
C                                       Graphics devices
 270  IF (NTKDEV.LE.0) GO TO 10
         INPOPS = NTKDEV
         WRITE (MSGBUF,1270)
         CALL INQINT (TTY, MSGBUF, 1, J1, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (IERR.LT.0) GO TO 270
         IF (J1.LE.0) GO TO 10
            NTKDEV = INPOPS
            GO TO 10
C-----------------------------------------------------------------------
C                                        file catalog
 300  WRITE (MSGBUF,1300)
      CALL INQINT (TTY, MSGBUF, 1, IVOL, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 300
 301  IF (UCTSIZ.LE.0) GO TO 305
         WRITE (MSGBUF,1302) ILUSER
         CALL INQINT (TTY, MSGBUF, 1, I, IERR)
         IF (IERR.LT.0) GO TO 301
         IF (IERR.GT.0) GO TO 990
         IF (I.GT.0) ILUSER = I
         IF (ILUSER.LE.0) GO TO 301
 305  IF ((IVOL.GE.1) .AND. (IVOL.LE.NVOL)) THEN
         IVOL1 = IVOL
         IVOL2 = IVOL
      ELSE
         IVOL1 = 1
         IVOL2 = NVOL
         END IF
      DO 330 IVOL = IVOL1,IVOL2
         NLUSER = ILUSER
         CALL ZPHFIL (ITYP, IVOL, 0, 0, PHNAME, IERR)
         CALL ZEXIST (IVOL, PHNAME, ISIZE, IERR)
         IF (IERR.NE.0) GO TO 940
         ISIZE = ISIZE - 1
         NL = 256 / 10
         ICMAX = ISIZE * NL
         IF (ICMAX.GT.46655) ICMAX = 46655
         NLUSER = 1
         CALL ZOPEN (LUN, FIND, IVOL, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL FILL (256, 0, BUFFER)
         BUFFER(1) = IVOL
         BUFFER(3) = ICMAX
         CALL ZDATE (BUFFER(4))
         CALL ZTIME (BUFFER(7))
         CALL ZDATE (BUFFER(10))
         CALL ZTIME (BUFFER(13))
         CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 930
         CALL FILL (16, 0, BUFFER)
         DO 310 I = 1,256,10
            BUFFER(I) = -1
 310        CONTINUE
         J1 = 2
         J2 = J1 + (ICMAX-1)/NL
         DO 320 IRNO = J1,J2
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 930
 320        CONTINUE
         CALL ZCLOSE (LUN, FIND, IERR)
 330     CONTINUE
      GO TO 900
C-----------------------------------------------------------------------
C                                       task data file
 350  WRITE (MSGBUF,1200)
      CALL INQINT (TTY, MSGBUF, 1, INPOPS, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 350
      CALL ZPHFIL (ITYP, 1, 0, 4, PHNAME, IERR)
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL ZFIO ('READ', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 920
      IMAX = NINTRN + 1 + NBATQS
      IF (NBATQS.LE.0) IMAX = NINTRN
      IF ((INPOPS.GE.1) .AND. (INPOPS.LE.IMAX)) THEN
         INPOP1 = INPOPS
         INPOP2 = INPOPS
      ELSE
         INPOP1 = 1
         INPOP2 = IMAX
         END IF
      J1 = 5 * (INPOP2-INPOP1+1)
      J = 5*INPOP1 - 4
      CALL FILL (J1, 0, BUFFER(J))
      BUFFER(256) = 4
      CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 930
      GO TO 950
C-----------------------------------------------------------------------
C                                        Accounting
 400  CALL ZPHFIL (ITYP, 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
      IF (IERR.NE.0) GO TO 940
      IR = ISIZE
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL FILL (256, 0, BUFFER)
      BUFFER(3) = 9
      BUFFER(4) = 256 / BUFFER(3)
      BUFFER(2) = IR * BUFFER(4) - 1
      CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 930
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 900
C-----------------------------------------------------------------------
C                                        TV device control
 450  WRITE (MSGBUF,1450)
      CALL INQINT (TTY, MSGBUF, 1, IC, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 450
      IVOL1 = IC
      IVOL2 = IC
      IF ((IC.GT.0) .AND. (IC.LE.LTVDEV)) GO TO 460
         IVOL1 = 1
         IVOL2 = LTVDEV
 460  DO 470 NTVDEV = IVOL1,IVOL2
         CALL YTVCIN
         CALL ZPHFIL (ITYP, 1, 0, NTVDEV, PHNAME, IERR)
         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL FILL (256, 0, BUFFER)
         CALL COPY (88, NGRAY, BUFFER)
         IRNO = 1
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 930
         IF (ISUNUM.LE.0) GO TO 465
            CALL FILL (256, 0, BUFFER)
            IRNO = 2
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 930
 465     CALL ZCLOSE (LUN, FIND, IERR)
 470     CONTINUE
      NTVDEV = LTVDEV
      GO TO 900
C-----------------------------------------------------------------------
C                                        Gripes
 500  CALL ZPHFIL (ITYP, 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
      IF (IERR.NE.0) GO TO 940
      IR = ISIZE
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL FILL (256, 0, BUFFER)
      BUFFER(1) = IR
      BUFFER(3) = 2
      BUFFER(4) = 1
      IRNO = 1
      CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 930
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 900
C-----------------------------------------------------------------------
C                                       Password file
 550  WRITE (MSGBUF,1550) USELIM
      CALL INQINT (TTY, MSGBUF, 2, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 550
      IUS1 = MAX (1, IARR(1))
      IUS2 = MIN (USELIM, IARR(2))
      IF (IUS2.LT.IUS1) IUS2 = USELIM
      CALL ZPHFIL (ITYP, 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      LPASS = ' '
      CALL PASENC (LPASS, APASS)
      I4T = 0
      J = 3
      J = 256 / J
      DO 560 I = IUS1,IUS2
         IRNO = (I-1) / J + 1
         IF (IRNO.EQ.I4T) GO TO 555
            IF (I4T.GT.0) CALL ZFIO ('WRIT', LUN, FIND, I4T, BUFFER,
     *         IERR)
            IF (IERR.NE.0) GO TO 930
            CALL ZFIO ('READ', LUN, FIND, IRNO, BUFFER, IERR)
            I4T = IRNO
            IF (IERR.NE.0) GO TO 920
 555     J1 = MOD (I-1, J)
         J1 = 3 * J1 + 1
         IF (I.NE.1) CALL RCOPY (3, APASS, RBUFFR(J1))
         IF (I.EQ.1) CALL PASENC (MPASS, RBUFFR(J1))
 560     CONTINUE
      IF (I4T.GT.0) CALL ZFIO ('WRIT', LUN, FIND, I4T, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 930
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 900
C-----------------------------------------------------------------------
C                                       Task communications (SHOW, TELL)
 600  WRITE (MSGBUF,1200)
      CALL INQINT (TTY, MSGBUF, 1, INPOPS, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 600
      CALL ZPHFIL (ITYP, 1, 0, 1, PHNAME, IERR)
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 910
      IF (INPOPS.LE.0) THEN
         CALL FILL (256, 0, BUFFER)
         DO 610 IRNO = 1,18
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 930
 610        CONTINUE
      ELSE
         IRNO = (INPOPS - 1) / 2 + 1
         CALL ZFIO ('READ', LUN, FIND, IRNO, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 920
         J = 128 * (INPOPS + 1 - 2*IRNO) + 1
         CALL FILL (128, 0, BUFFER(J))
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 930
         END IF
      GO TO 950
C-----------------------------------------------------------------------
C                                        messages & close file
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (7)
      GO TO 10
 910  WRITE (MSGTXT,1910) IERR
      CALL MSGWRT (7)
      GO TO 10
 920  WRITE (MSGTXT,1920) IERR
      CALL MSGWRT (7)
      GO TO 950
 930  WRITE (MSGTXT,1930) IERR
      CALL MSGWRT (7)
      GO TO 950
 940  WRITE (MSGTXT,1940) IERR
      CALL MSGWRT (7)
C
 950  CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 900
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 995  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1005 FORMAT ('ERROR',I7,' FROM ZEXIST ON PASSWORD FILE')
 1010 FORMAT ('Quit Create Init Destroy (A4)')
 1020 FORMAT ('Type Vol Seq Ver User (A2, 4 of I)   Default user',I5)
 1022 FORMAT ('Number 256-word records (I)  Default=',I5)
 1050 FORMAT ('Filetype (A2)')
 1100 FORMAT ('Q# , first thru last jobnos (3 I)')
 1150 FORMAT ('Q# POPS# (2 I) 0''S => All')
 1200 FORMAT ('POPS# (I) 0''S => All')
 1230 FORMAT ('About to initialize ', I3, ' TV image catalogs...')
 1250 FORMAT ('TV #',I3,', which plane? (I) 0 => All, -1 => None,',
     *   ' -2 => Quit')
 1270 FORMAT ('Graphics device # (I) 0 => All, -1 => None')
 1300 FORMAT ('Vol (I) 0 => All')
 1302 FORMAT ('User number (I) default=',I4)
 1450 FORMAT ('TV number (I) 0 => all')
 1502 FORMAT ('User number (I) default=',I4)
 1550 FORMAT ('User number range (2 I) default= 0, ',I4)
 1900 FORMAT ('ERROR CODE = ',I7)
 1910 FORMAT ('OPEN ERROR',I7)
 1920 FORMAT ('READ ERROR',I7)
 1930 FORMAT ('WRITE ERROR',I7)
 1940 FORMAT ('ZEXIST ERROR',I7)
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
