      SUBROUTINE ALGSTB (IFIELD, SCRGRD, DISKI, CNOSCI, DISKO, CNOSCO,
     *   CHANEL, NCHAN, CATR, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Interpolates model visibility from a grid and subtracts from uv data.
C# UV Modeling AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2001, 2003, 2006, 2008-2010, 2012
C;  Copyright (C) 2014-2015, 2018
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
C   ALGSTB interpolates model visibility data from a grid and subtracts
C   it from the observed visibilities.  UV data may be in any sort order
C   If AP memory is large enough to hold all gridded model, ALGSTB runs
C   much faster.
C   Inputs:
C      IFIELD   I       Field number.
C      SCRGRD   I       /CFILES/ file number for grid file.
C      DISKI    I       Input file disk number for catalogd files,
C                       .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I       Input file catalog slot number or /CFILES/
C                       scratch file number.
C      DISKO    I       Output file disk number for catalogd files,
C                       .LE. 0 => /CFILES/ scratch file.
C      CNOSCO   I       Output file catalog slot number or /CFILES/
C                       scratch file number.
C      CHANEL   I       First channel number.
C      NCHAN    I       Number of channels to process.
C      CATR     R(256)  UV data catalog header record.
C      JBUFSZ   I       Size in bytes of buffers. Dimension of
C                       BUFF1,2,3  must be at least 4096 words.
C   Inputs: from commons
C      LREC     I       length in words of vis record.
C      NVIS     I       number of vis records
C      FREQG    D(*)    Frequencies of IF and channels in same order
C                       as occurs in the data.
C   Output:
C      BUFF1    R(*)    Working buffer
C      BUFF2    R(*)    Working buffer
C      BUFF3    R(*)    Working buffer
C      IRET     I       Return code, 0 => ok, otherwise not.
C   The AP grid file must contain the gridded UV model for this pass.
C   The UV points are read in and the model is subtracted from them.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, SCRGRD, DISKI, CNOSCI, DISKO, CNOSCO, CHANEL,
     *   NCHAN, JBUFSZ, IRET
      REAL       BUFF1(*), BUFF2(*), BUFF3(*)
C
      INTEGER   CJTBSZ, MINT, TPBFMX
C                                       CJTBSZ = conj. table size
C                                       MINT   = Interpol. support size.
C                                       TPBFMX = Temp Buffer size
      PARAMETER (CJTBSZ=4000, MINT=11, TPBFMX=20000)
      INTEGER   VISRED, NIOUT, IDISK, ICNOSC, INDS(3), NX, NY, ILENBU,
     *   IUMIN, IUMAX, USTOP, USTRT, IU, U, IVIS, ICONJ, I, NIO, LENBU,
     *   FLIST(22), CNJPTR(CJTBSZ), M, MO2, LOCHAN, NCHGRD, MSGINC,
     *   NROGRD, MXROGD, KAP, CX, VIS, UV, LLREC, MM, WORK, INDEX, LROW,
     *   FLAG, VISOFF, NS, APSIZE, MAXREC, CNT, IFACT, ITEMP1, ITEMP2,
     *   INPTR, OUTPTR, JNPTR, KNPTR, LNPTR, NPOINT, JNCS, EXCESS,
     *   LOGRID, JNCF, LRPARM, ROWSIZ, IDATA, MAXU, RPASS, NRPASS,
     *   NROWS, NXO2, EROW, ERRCNT, IXTEMP(3), MSGSAV, HIC, LOC, J, K,
     *   IUFMAX, IULO, IUHI, LSTRT, LSTOP, NROW, END1, LUMAX, LUMIN,
     *   NEED, UCOUNT, UTARG, II, JJ, KK
      LOGICAL   ALLROW, ONCE, DOROT, DOFLAG
      REAL      XTEMP(7), FACT2(4), CATR(256), ZSCLV, ZSCLW, ZSCLU,
     *   SCALU, SCALV, FFRAC, FRSTU, UUMAX, VVMAX, UIN, VIN, UMIN, UMAX,
     *   TEMPBF(TPBFMX), UUU, UMAT(3,3), PMAT(3,3), DDX(3),
     *   RMAT(3,3), DMAT(3,3), RULO, RUHI, UVWS(3,TPBFMX)
      DOUBLE PRECISION GFACT, MAXFRQ, XRA, XDEC, MINFRQ, MXFACT, MNFACT
      CHARACTER CSTOK*1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:DAPM.INC'
      SAVE ONCE, ERRCNT
      DATA ONCE /.FALSE./, ERRCNT /0/
C-----------------------------------------------------------------------
      IRET = 0
      DOFLAG = ERRCNT.EQ.0
C                                       Rotate?
      DOROT = (ABS (SSROT).GT.1.0E-10) .OR. (ABS (CCROT-1.0).GT.1.0E-4)
      APSIZE = PSAPNW * 1024
      NX = ABS (FLDSZ(1,IFIELD) * OSFX) + 0.1
      NY = ABS (FLDSZ(2,IFIELD) * OSFY) + 0.1
      ZSCLU = SCLUG(IFIELD) * OSFX
      ZSCLV = SCLVG(IFIELD) * OSFY
      ZSCLW = SCLWG(IFIELD)
      NXO2 = NX / 2
C                                       Set AP Pointers; IFACT=cellsiz
      IFACT = 2
      LROW = NY
      ROWSIZ = IFACT * LROW
C                                       Set up for APIO
      CALL FILL (22, 0, FLIST)
      FLIST(1) = LUNS(4)
      FLIST(2) = SCRGRD
      FLIST(5) = ROWSIZ
      FLIST(6) = NXO2 + 1
      FLIST(9) = 1
      FLIST(11) = FLIST(5)
      FLIST(12) = 1
      FLIST(13) = JBUFSZ
C                                       Get un-compressed UV increments
      IF (DATDIV) CALL PRMUPD
      IF (COMPDT) THEN
         ITEMP1 = LREC
         ITEMP2 = NRPARM
         LREC = SCLREC
         NRPARM = SCRPRM
         END IF
C                                       Correlator scaling table.
      MSGSAV = MSGSUP
      MSGSUP = 31900
      CALL GETCTL (CATR, FACT2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      CSTOK = MSGTXT(1:1)
      NS = NSTOK
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF,
     *   LRPARM, LLREC)
C                                       Set I/O visibility count
      ILENBU = ((MIN(JBUFSZ,TPBFMX*2) - 2 * NBPS) / 2) / LLREC - 2
      ILENBU = MAX (ILENBU, 1)
      LENBU  = ILENBU
      IDISK = DISKI
      ICNOSC = CNOSCI
C                                       Grab ap
      M = MINT
      MAXREC = 256
      NEED = MAXREC*(LLREC+4) + 200*M + 30 + NCHAN + (NXO2+M)*ROWSIZ
      NEED = NEED / 1024 + 2
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         IRET = 8
         MSGTXT = 'ALGSTB UNABLE TO GET DESIRED MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      APSIZE = 1024 * PSAPNW
C                                       Calc extra ap for interp func
      MAXREC = MIN (2048, (APSIZE/30)/(LLREC+4)) + 5
      MAXREC = MAX (15, MAXREC)
      EXCESS = APSIZE - MAXREC*(LLREC+4) - 200*MINT - 30 - NCHAN
C                                       How big can interp func be?
      M = EXCESS / (ROWSIZ + 200)
C                                       M between mint/2 and mint
      M = MAX (MINT/2, MIN (M, MINT))
C                                       M must be odd, round down
      M = (2 * ((M-1)/2)) + 1
      MO2 = M / 2
C                                       Extra ap goes to extra rows
      EXCESS = APSIZE - MAXREC*(LLREC+4) - 200*M - 30 - NCHAN
      MXROGD = (EXCESS/ROWSIZ) - 1
      IF (MXROGD.LT.M) THEN
         IRET = 8
         MSGTXT = 'ALGSTB UNABLE TO GET ENOUGH MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Grid no more than all rows
      MXROGD = MIN (MXROGD, NXO2+M)
C                                       Set ap pointers
      LOGRID = APSIZE - ((MXROGD+1) * ROWSIZ) - 1
      CX = LOGRID - (200 * (M+1)) - 1
      MM = M
      FLAG = 1
      IF ((KSTOK.EQ.3) .AND. (ICOR0.LE.0)) FLAG = -1
C                                       model only
      IF (FACGRD(2).EQ.0.0) THEN
         UUU = 0.0
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
         FACT2(3) = FACT2(3) * FACGRD(1)
         FACT2(4) = FACT2(4) * FACGRD(1)
C                                       Scale FACT2 by -FACGRD
C                                       data - model
      ELSE
         UUU = 1.0
         FACT2(1) = -FACT2(1) * FACGRD(1)
         FACT2(2) = -FACT2(2) * FACGRD(1)
         FACT2(3) = -FACT2(3) * FACGRD(1)
         FACT2(4) = -FACT2(4) * FACGRD(1)
         END IF
C                                       If fft scale by 0.5
      IF (DOFFT) THEN
         FACT2(1) = FACT2(1) * 0.5
         FACT2(2) = FACT2(2) * 0.5
         FACT2(3) = FACT2(3) * 0.5
         FACT2(4) = FACT2(4) * 0.5
         END IF
C                                       Prepare for bs loop
      DDX(1) = DXCG(IFIELD)
      DDX(2) = DYCG(IFIELD)
      DDX(3) = DZCG(IFIELD)
      CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD), MAPROT,
     *   XRA, XDEC)
      IF (DO3DIM) THEN
         CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
         CALL RFILL (9, 0.0, RMAT)
         CALL RFILL (9, 0.0, DMAT)
         RMAT(1,1) = CCROT
         RMAT(1,2) = SSROT
         RMAT(2,1) = -SSROT
         RMAT(2,2) = CCROT
         RMAT(3,3) = 1
         DO 20 I = 1,3
            DO 15 J = 1,3
               DO 10 K = 1,3
                  DMAT(I,J) = DMAT(I,J) + PMAT(I,K) * RMAT(K,J)
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
         CALL PRJMUL (2, DDX, DMAT, DDX)
      ELSE
         CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
         END IF
C                                       self defense
      DO 30 I = 1,3
         DO 25 J = 1,3
            IF (UMATS(I,J,1).NE.0.0) GO TO 35
 25         CONTINUE
 30      CONTINUE
      MSGTXT = 'ALGSTB: ROTATION MATRIX UMATS IS 0, FIELD 1'
      CALL MSGWRT (6)
      MSGTXT = 'ALGSTB: THIS IS A SERIOUS ERROR SINCE MFIELD > 1'
      IF (MFIELD.GT.1) CALL MSGWRT (7)
      CALL RCOPY (9, UMAT, UMATS)
C                                       Get maximum u
 35   CALL UVPROT ('READ', IDISK, ICNOSC, LUNS(2), INDS(2), MFIELD,
     *   UMATS, CCROT, SSROT, NVIS, LREC, ILENBU, JBUFSZ, BUFF1, NIO,
     *   INPTR, FRSTU, DISKO, CNOSCO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZCLOSE (LUNS(2), INDS(2), IRET)
C                                       Find Max Frequency
      MAXFRQ = FREQG(CHANEL)
      MINFRQ = FREQG(CHANEL)
      DO 40 I = 2,NCHAN
         BUFF3(I-1) = 0.0
         IF (FREQG(CHANEL+I-1).GT.0.0D0) THEN
            MAXFRQ = MAX (MAXFRQ, FREQG(CHANEL+I-1))
            MINFRQ = MIN (MINFRQ, FREQG(CHANEL+I-1))
            BUFF3(I-1) = FREQG(CHANEL+I-1)/FREQG(CHANEL+I-2) - 1.0D0
            END IF
 40      CONTINUE
C                                       Scale by Max Freq. to cells
      MAXU   = ABS (FRSTU * MAXFRQ / FREQ * ZSCLU) + 0.99
C                                       Limit u to max that will fit
      MAXU   = MIN (MAXU, NXO2)
C                                       Calc number needed
      NROGRD = MAXU + M
C                                       Do all rows fit in ap?
      ALLROW = MXROGD.GE.NROGRD
C                                       Number of rows in a pass
      NROWS  = MIN (NROGRD, MXROGD)
C                                       If all rows fit, 1 pass
      IF (ALLROW) THEN
         NRPASS = 1
         WRITE (MSGTXT,1040) NROWS, MXROGD
C                                       Else, how many row passes?
      ELSE
         NRPASS = (MAXU + 1)/(NROWS - M + 1)
         IF (NRPASS*(NROWS-M+1).LT.(MAXU+1)) NRPASS = NRPASS + 1
         WRITE (MSGTXT,1041) NROWS, NROGRD
         END IF
      IF (.NOT.ONCE) CALL MSGWRT (2)
      VISRED = 0
      LOCHAN = 1
      NCHGRD = NCHAN
      LOC = CHANEL + LOCHAN - 1
      HIC = LOC + NCHGRD - 1
      IF (MFIELD.LE.1) THEN
         WRITE (MSGTXT,1045) CSTOK, LOC, HIC
      ELSE
         WRITE (MSGTXT,1046) IFIELD, CSTOK, LOC, HIC
         END IF
      CALL MSGWRT (2)
C                                       Determine where data starts.
      FLIST(10) = MIN (MAXU+MO2+1, NXO2+1)
C                                       Get frequency scaling factors.
      FFRAC = (MINFRQ / FREQ) - 1.0D0
      GFACT = 1.0D0 / ((1.0D0 + FFRAC) * ZSCLU)
      MNFACT = ZSCLU * MINFRQ / FREQ
      MXFACT = ZSCLU * MAXFRQ / FREQ
      FFRAC = (MAXFRQ / FREQ) - 1.0D0
      SCALU = FFRAC * ZSCLU + ZSCLU
      SCALV = FFRAC * ZSCLV + ZSCLV
C                                       Set limits on u,v - don't get
C                                       Within mint/2+1 of edge.
      UUMAX = ABS (((NX/2) - (MINT/2+1)) / SCALU)
      VVMAX = ABS (((NY/2) - (MINT/2+1)) / SCALV)
C                                       Setup ap griding constants
      FFRAC = (FREQG(LOCHAN+CHANEL-1) / FREQ) - 1.0D0
      XTEMP(1) = (ZSCLU + FFRAC * ZSCLU)
      XTEMP(2) = (ZSCLV + FFRAC * ZSCLV)
      XTEMP(3) = DDX(1) / ZSCLU
      XTEMP(4) = DDX(2) / ZSCLV
      XTEMP(5) = DDX(3) + DDX(3)*FFRAC
      XTEMP(6) = 0.0
      XTEMP(7) = 0.0
      IXTEMP(1) = 0
      IXTEMP(2) = LOGRID
      IXTEMP(3) = CX
      CALL QPUT (XTEMP, 0, 7, 2)
      CALL QPUT (IXTEMP, 7, 3, 1)
C                                       Correlator scaling table.
      CALL QPUT (FACT2, 16, NS, 2)
      CALL QPUT (1.0, 15, 1, 2)
C                                       Frequency scaling table.
      BUFF3(NCHGRD) = 0.0
      CALL QPUT (BUFF3, 20, NCHGRD, 2)
C                                       Set interpolation tables
      CALL INTPFN (CX, M, BUFF3, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save space for 4 word work array.
      MAXREC = (CX - 20 - NS - NCHGRD) / (LLREC+4) - 5
C                                       No more recs than conj table.
      IF (MAXREC.GT.CJTBSZ) MAXREC = CJTBSZ
C                                       More ap pointers.
      WORK = 20 + NCHGRD
      UV = WORK + 4 + (MAXREC * 4)
C                                       Correction for stokes.
      VISOFF = LRPARM + ((CHANEL-1) + (LOCHAN-1))*JNCF + (VOFF / INCS) *
     *   JNCS
      VIS = UV + VISOFF
C                                       Set location of data inp ap
      IDATA = UV
      IUMAX = MAXU
      RPASS = 0
      LUMAX = 10 * MAXU
      LUMIN = LUMAX + 1
C                                       Row pass loop point
 50   RPASS = RPASS + 1
         IUMIN = MAX (IUMAX - (NROWS-M), 0)
         IUFMAX = -1000
         LSTRT = USTRT
         LSTOP = USTOP
C                                       Load ALL rows
         USTRT = MAX (IUMIN - MO2, 0)
         USTOP = MIN (IUMAX+MO2, NXO2)
C                                       move ones previously loaded
         IF ((RPASS.GT.1) .AND. (USTOP.GE.LSTRT)) THEN
C                                       wait for AP to FINISH
            CALL QWAIT
C                                       must move from top to bottom
            NROW = USTOP - LSTRT + 1
            DO 60 I = 1,NROW
               END1 = LOGRID + ((NROW-I)*ROWSIZ)
               EROW = LOGRID + ((USTOP-(IUMIN-MO2))*ROWSIZ)
               CALL QVMOV (END1, 1, EROW, 1, ROWSIZ)
               CALL QWAIT
               USTOP = USTOP - 1
 60            CONTINUE
C                                       clear rest for gridding
            CALL QVCLR (LOGRID, 1, EROW-LOGRID)
            CALL QWAIT
            END IF
C                                       Load grided uv values in ap
         CALL APLOAD (USTRT, USTOP, IUMIN-MO2, ROWSIZ, LOGRID, APSIZE,
     *      FLIST, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       When near end of rows
         IF (IUMIN.EQ.0) THEN
            DO 90 IU = 1, MO2
C                                       Convert to decreasing u
               U = MO2 - IU + 1
C                                       U=0 at lrow/2
C                                       U<0 rows from complex cong u>0
C                                       Index points to end of u row
               INDEX = LOGRID + ((U+MO2+1)*ROWSIZ) - IFACT
C                                       Erow points to start of -u row
               EROW  = LOGRID + ((MO2-U)*ROWSIZ)
               CALL QCVCON (INDEX, -IFACT, EROW+IFACT, IFACT, LROW-1)
C                                       Index points to start same u row
               INDEX = INDEX - ROWSIZ + IFACT
C                                       Conjugate a single pixel
               CALL QCVCON (INDEX, IFACT, EROW, IFACT, 1)
 90            CONTINUE
            END IF
C                                       Set min and max this pass
         UMAX = (IUMAX + 0.5) * GFACT
         UMIN = (IUMIN - 0.5) * GFACT
         OUTPTR = 1
         INPTR  = 1
         ILENBU = MIN (LENBU, MAXREC)
C                                       Set output file name.
         CALL UVPREP ('WRIT', DISKO, CNOSCO, LUNS(3), INDS(3), NVIS,
     *      LREC, ILENBU, JBUFSZ, BUFF3, NIOUT, OUTPTR, FRSTU, IRET)
         IF (IRET.NE.0) GO TO 999
         NIOUT = ILENBU
         ILENBU = MIN (LENBU, MAXREC)
C                                       Prepare to read through data
         CALL UVPREP ('READ', IDISK, ICNOSC, LUNS(2), INDS(2), NVIS,
     *      LREC, ILENBU, JBUFSZ, BUFF1, NIO, INPTR, FRSTU, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Reset input to output file.
         IDISK = DISKO
         ICNOSC = CNOSCO
C                                       Count points this pass
         NPOINT = 0
         UCOUNT = NIO
         UTARG = 1
C                                       Loop forever, read all uvdata
 100     CONTINUE
            IF (UCOUNT.GT.UTARG) THEN
               WRITE (MSGTXT,1100) UTARG
               CALL MSGWRT (2)
               UTARG = UTARG + 200000
               END IF
C                                       Rotate, save init u,v,w
            JNPTR = INPTR
            DO 110 I =1,NIO
               UVWS(1,I) = BUFF1(JNPTR+ILOCU)
               UVWS(2,I) = BUFF1(JNPTR+ILOCU+1)
               UVWS(3,I) = BUFF1(JNPTR+ILOCU+2)
               CALL PRJMUL (1, BUFF1(JNPTR+ILOCU), UMAT,
     *            BUFF1(JNPTR+ILOCU))
               JNPTR = JNPTR + LREC
 110           CONTINUE
C                                       Start read from beginning of uvs
            JNPTR = INPTR
C                                       Put uvdata back at beginning
            KNPTR = INPTR
C                                       Use start of output buffer
            LNPTR = OUTPTR
C                                       Count points this buffer
            CNT = 0
C                                       Do all points in buffer
            DO 200 IVIS = 1,NIO
C                                       Input u, v values
               UIN = ABS (BUFF1(JNPTR+ILOCU))
               VIN = ABS (BUFF1(JNPTR+ILOCV))
               IULO = MNFACT * UIN + 0.5
               IUHI = MXFACT * UIN + 0.5
               RULO = MNFACT * UIN
               RUHI = MXFACT * UIN
C                                       uv outside range.
               IF ((UIN.GT.UUMAX) .OR. (VIN.GT.VVMAX)) THEN
                  CALL  UVLIMT (0.0, 1.0E20, UUMAX, VVMAX, IVIS,
     *               UIN, VIN, ERRCNT)
C                                       Reset u to within range
                  UIN = MAXU * GFACT
                  UIN = MIN (UUMAX, UIN) * 0.99
C                                       Put location back in buffer
                  BUFF1(JNPTR+ILOCU) = UIN
                  BUFF1(JNPTR+ILOCV) = 0.0
C                                       Zero data
                  CALL RFILL (LREC-VISOFF, 0.0, BUFF1(JNPTR+VISOFF))
C                                       Zero data leave weights
               ELSE IF (UUU.EQ.0.0) THEN
                  KK = JNPTR + VISOFF - 1
                  JJ = LREC - VISOFF
C                                       compressed
                  IF (LREC.LT.LLREC) THEN
                     CALL FILL (LREC-NRPARM, 0, BUFF1(JNPTR+NRPARM))
                  ELSE
                     DO 120 II = 1,JJ,3
                        BUFF1(KK+II) = 0.0
                        BUFF1(KK+II+1) = 0.0
 120                    CONTINUE
                     END IF
                  END IF
C                                       If datum on current rows
C              IF (UIN.GT.UMIN .AND. (UIN.LE.UMAX.OR.RPASS.EQ.1)) THEN
               IF (((IUHI.LE.IUMAX) .OR. (RPASS.EQ.1)) .AND.
     *            (IULO.GE.IUMIN) .AND. ((RPASS.EQ.1) .OR.
     *            (IUHI.GT.LUMAX) .OR. (IULO.LT.LUMIN))) THEN
C                                       Keep count of number to process
                  CNT = CNT + 1
C                                       Store output point location
                  CNJPTR(CNT) = LNPTR
C                                       If u neg. then flip
                  IF (BUFF1(JNPTR+ILOCU).LT.0.0) THEN
C                                       Enter in cnjptr table.
                     CNJPTR(CNT) = -LNPTR
C                                       Flip u, v, w, conjugate data.
                     CALL UVCONJ (VISOFF, JNCF, NCHGRD, NSTOK,
     *                  JNCS, BUFF1(JNPTR))
C                                       End if u < 0, conjugate
                     END IF
C                                       If more than 1 pass
                  IF (.NOT.ALLROW) THEN
C                                       Put data in buff for processing
                     CALL COPY (LREC, BUFF1(JNPTR), BUFF1(KNPTR))
C                                       Update output pointer
                     KNPTR = KNPTR + LREC
                     END IF
C                                       Else not in current rows
               ELSE
                  IF (IULO.LT.IUMIN) IUFMAX = MAX (IUFMAX, IUHI)
C                                       Put in output buffer
                  CALL COPY (LREC, BUFF1(JNPTR), BUFF3(LNPTR))
C                                       End if on current rows
                  END IF
C                                       Set new input and output pos
               JNPTR = JNPTR + LREC
               LNPTR = LNPTR + LREC
C                                       End for all vis recs read
 200           CONTINUE
C                                       Keep track of total
            VISRED = VISRED + CNT
C                                       Count points in range each pass
            NPOINT = NPOINT + CNT
C                                       If points were found
            IF (CNT.GT.0) THEN
C                                       Wait for ap, then load into ap
               CALL QWR
C                                       Uncompress Vis and put in AP
               CALL BUFPUT (CNT, NRPARM, LREC, LLREC, IDATA,
     *            BUFF1(INPTR), TEMPBF(1))
C                                       Low row to ap
               IXTEMP(1) = IUMIN
               IXTEMP(2) = LOGRID
               CALL QPUT (IXTEMP, 7, 3, 1)
               CALL QWAIT
               CALL QUVIN (UV, VIS, WORK, LLREC, MM, LROW, NS,
     *            JNCS, NCHGRD, JNCF, CNT, FLAG)
               CALL QWR
C                                       Read out interpolated data
C                                       If all data in ap
               IF (ALLROW) THEN
C                                       Get UVs from AP (maybe pack UV)
                  CALL BUFGET (CNT, NRPARM, LREC, LLREC, IDATA,
     *               BUFF3(OUTPTR), TEMPBF(1))
C                                       Else get some data
               ELSE
C                                       Get UVs from AP (maybe pack UV)
                  CALL BUFGET (CNT, NRPARM, LREC, LLREC, IDATA,
     *               BUFF1(INPTR), TEMPBF(1))
                  JNPTR = INPTR
C                                       For all points
                  DO 400 I = 1,CNT
C                                       Get output location of data
                     LNPTR = ABS(CNJPTR(I))
C                                       Move from input to output
                     CALL COPY (LREC, BUFF1(JNPTR), BUFF3(LNPTR))
C                                       Start location of next vis rec.
                     JNPTR = JNPTR + LREC
 400                 CONTINUE
C                                       End if not all in ap
                  END IF
C                                       For all points, conjugate
               DO 500 ICONJ = 1, CNT
C                                       If point was conjugated
                  IF (CNJPTR(ICONJ).LT.0) THEN
C                                       Get position
                     LNPTR = -CNJPTR(ICONJ)
C                                       Flip u, v, w, conjugate data.
                     CALL UVCONJ (VISOFF, JNCF, NCHGRD, NSTOK, JNCS,
     *                  BUFF3(LNPTR))
                     END IF
 500              CONTINUE
C                                       End if points this pass
               END IF
C                                       restore u,v,w
            JNPTR = OUTPTR
            DO 520 I = 1,NIO
               BUFF3(JNPTR+ILOCU) = UVWS(1,I)
               BUFF3(JNPTR+ILOCU+1) = UVWS(2,I)
               BUFF3(JNPTR+ILOCU+2) = UVWS(3,I)
               JNPTR = JNPTR + LREC
 520           CONTINUE
C                                       Write all nio points read
            NIOUT = NIO
C                                       Write data to disk
            CALL UVDISK ('WRIT', LUNS(3), INDS(3), BUFF3, NIOUT,
     *         OUTPTR, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET
               GO TO 995
               END IF
C                                       Read a new uvdata buffer
            CALL UVDISK ('READ', LUNS(2), INDS(2), BUFF1, NIO, INPTR,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1300) IRET
               GO TO 995
               END IF
            UCOUNT = UCOUNT + NIO
C                                       If more data, loop
            IF (NIO.GT.0) GO TO 100
C                                       Jump here on end uv data
C                                       Write Diag messages ONLY once
C                                       Print only 10 Messages
         IF (.NOT.ONCE) THEN
            WRITE(MSGTXT,1104,ERR=680) RPASS, IUMAX, IUMIN, NPOINT
            MSGINC = (NRPASS/10) + 1
 680        IF (((RPASS/MSGINC)*MSGINC).EQ.RPASS) CALL MSGWRT (2)
            END IF
C                                       Finish write.
         NIOUT = 0
         CALL UVDISK ('FLSH', LUNS(3), INDS(3), BUFF3, NIOUT, OUTPTR,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1500) IRET, I
            GO TO 995
            END IF
C                                       Close files
         CALL ZCLOSE (LUNS(2), INDS(2), IRET)
         CALL ZCLOSE (LUNS(3), INDS(3), IRET)
         LUMIN = IUMIN
         LUMAX = IUMAX
C                                       end of rows loop: do more?
         IF (IUFMAX.GT.0) THEN
            IUMAX = IUFMAX
            GO TO 50
            END IF
C                                       Check that written all data read
      IF (NVIS.NE.VISRED) THEN
         WRITE (MSGTXT,1600) NVIS, VISRED
         CALL MSGWRT (8)
         END IF
      IF ((DOFLAG) .AND. (ERRCNT.GT.0)) THEN
         MSGTXT = '**************************************************'
     *     // '*****'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1601) ERRCNT
         CALL MSGWRT (8)
         MSGTXT = '**************************************************'
     *     // '*****'
         CALL MSGWRT (8)
         END IF
C                                       re-set pointers to beginning
C                                       of the grid file.
      CALL APIO ('INIT', FLIST, LOGRID, BUFF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1400) IRET
         GO TO 995
         END IF
C                                       Give up AP.
      CALL QRLSE
C                                       write diag messages once only
C                                       DEBUG
C     ONCE = .TRUE.
C                                       Close grid file.
      CALL APIO ('CLOS', FLIST, LOGRID, BUFF2, IRET)
      IF (COMPDT) THEN
         LREC = ITEMP1
         NRPARM = ITEMP2
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('ALGSTB: All ',I5,' Rows In AP (Max  ',I5,')')
 1041 FORMAT ('ALGSTB: Only',I5,' Rows In AP (Need ',I5,')')
 1045 FORMAT ('ALGSTB: ',A1,'pol gridded model subtraction, chans',I5,
     *   ' through',I5)
 1046 FORMAT ('ALGSTB field',I5,1X,A1,'pol gridded model subtraction',
     *   ' chns',I5,'-',I5)
 1100 FORMAT ('ALGSTB: at visibility number',I12)
 1104 FORMAT ('ALGSTB: Pass',I4,';',I5,'-',I4,' Cells, with',I9,
     *        ' Pts')
 1200 FORMAT ('ALGSTB: WRITE ERROR IN VISIBILITY FILE, IER=',I6)
 1300 FORMAT ('ALGSTB: READ  ERROR IN VISIBILITY FILE, IER=',I6)
 1400 FORMAT ('ALGSTB: TROUBLE INIT GRID FILE, IER=',I5)
 1500 FORMAT ('ALGSTB: ERROR',I5,' READING GRID ROW ',I5)
 1600 FORMAT ('ALGSTB: WARNING! Misplaced Data, NVIS = ',I8,
     *   ' WRITTEN =',I8)
 1601 FORMAT ('ALGSTB:',I10,' POINTS FLAGGED FOR BEING OFF THE GRID')
      END
