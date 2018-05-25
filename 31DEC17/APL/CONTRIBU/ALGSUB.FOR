      SUBROUTINE ALGSUB (IFIELD, SCRGRD, SCRWRK, DISKI, CNOSCI, DISKO,
     *   CNOSCO, CHANEL, NCHAN, CATR, JBUFSZ, BUFF1, BUFF2, BUFF3,
     *   IRET)
C-----------------------------------------------------------------------
C! Interpolates model visibility from a grid and subtracts from uv data.
C# UV Modeling AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2003, 2006, 2008
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
C   ALGSUB interpolates model visibility data from a grid and subtracts
C   it from the observed visibilities.  Data must be in 'XY' sort order
C   (not checked).
C   Inputs:
C      IFIELD   I       Field number.
C      SCRGRD   I       /CFILES/ file number for grid file.
C      SCRWRK   I       /CFILES/ file number for work file
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
C-----------------------------------------------------------------------
      INTEGER   IFIELD, SCRGRD, SCRWRK, DISKI, CNOSCI, DISKO, CNOSCO,
     *   CHANEL, NCHAN, JBUFSZ, IRET
      REAL       BUFF1(*), BUFF2(*), BUFF3(*)
C
C                                       CJTBSZ = conj. table size
      INTEGER   CJTBSZ
      PARAMETER (CJTBSZ=5000)
      INTEGER   VO, BO, VISOUT, VISRED, NIOUT, KLIM, IDISK, ICNOSC, IM,
     *   BIND2, BIND3, INDS2, INDS3, NX, NX2, NY, ILENBU, IFACT, IU, JU,
     *   I, NIO, FLIST(22), VOL, MINT, HICHAN, LIMISJ, NWRITE, IU1, IU2,
     *   CNJPTR(CJTBSZ), KINT, LIMIT, LOCHAN, NCHGRD, NROGRD, MXROGD,
     *   KAP, GRID, CX, VIS, END1, END2, NMOV, UV, LLREC, LROW, MM,
     *   WORK, INDEX, ONENY, FLAG, VISOFF, LVIS, NS, APMAXW, MAXREC,
     *   CNT, INDCNJ, INPTR, II, NPOINT, INCNT, LIM, JNPTR, NCONJ,
     *   MAXCNJ, IINCS, FCONJ, EXCESS, LOGRID, NF, IINCF, I4T1, I4T2,
     *   I4T3, I4SIZ, TOLROW, ITEMP, IDATA, IXTEMP(3), MSGSAV
      LOGICAL   T, F, ENDROW, CHKROL, DOROT
      REAL      XTEMP(7), UMIN, FACT2(2), XMAX, CATR(256), ZSCLV,
     *   ZSCLW, ZSCLU, SCALU, SCALV, FFRAC, FRSTU, UUMAX, VVMAX, DDX,
     *   DDY, UTEMP, GFAC2, UUU
      DOUBLE PRECISION GFACT, FREQLO, FREQHI
      CHARACTER NAME*48, CSTOK*1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C                                       MAXCNJ = size of conjugate array
      DATA MAXCNJ /CJTBSZ/
C                                       MINT = Interpolation support
C                                       size.
      DATA MINT /11/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check rotation, this routine
C                                       can't do it.
      DOROT = (ABS (SSROT).GT.1.0E-6) .OR. (ABS (CCROT-1.0).GT.1.0E-4)
      IF ((DOROT) .OR. (DO3DIM)) THEN
         IRET=5
         MSGTXT = 'ALGSUB: CANNOT HANDLE ROTATED OR 3D IMAGE'
         GO TO 995
         END IF
      NCONJ = 0
      CHKROL = F
      FFRAC = 0.0
      NX = FLDSZ(1,IFIELD) * OSFX + 0.1
      NY = FLDSZ(2,IFIELD) * OSFY + 0.1
      ZSCLU = SCLUG(IFIELD) * OSFX
      ZSCLV = SCLVG(IFIELD) * OSFY
      ZSCLW = SCLWG(IFIELD)
      NX2 = NX / 2 + 1
C                                       Set up for APIO
      CALL FILL (22, 0, FLIST)
      FLIST(1) = LUNS(4)
      FLIST(2) = SCRGRD
      FLIST(5) = NY * 2
      FLIST(6) = NX2
      FLIST(9) = 1
      FLIST(11) = FLIST(5)
      FLIST(12) = 1
      FLIST(13) = JBUFSZ
C                                       Set other values
      ONENY = NY
      LLREC = LREC
      IINCS = INCS
      IINCF = INCF
      IF ((INCIF.GT.0) .AND. (INCIF.LT.INCF)) IINCF = INCIF
      GFACT = 1.0D0 / ((1.0D0 + FFRAC) * ZSCLU)
      SCALU = FFRAC * ZSCLU + ZSCLU
      SCALV = FFRAC * ZSCLV + ZSCLV
C                                       Set I/O length.
      ILENBU = ((JBUFSZ - 2 * NBPS) / 2) / LREC - 2
      ILENBU = MAX (ILENBU, 1)
      IDISK = DISKI
      ICNOSC = CNOSCI
C                                       Loop, making sure problem will
C                                       fit.
      KINT = MINT
      I4T1 = KINT * 2 * NY
      I4T2 = 2 * (KINT + 1) * 100
      I4T3 = (LREC + 4) * ILENBU
C                                       Compute size needed.
      I4SIZ = 100 + I4T1 + I4T2 + I4T3
      APMAXW = I4SIZ / 1024 + 2
C                                       Grab AP
      CALL QINIT (APMAXW, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LT.APMAXW)) THEN
         IRET = 8
         MSGTXT = 'ALGSUB CANNOT GET DESIRED MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Check if fits.
      APMAXW = PSAPNW * 1024
C                                       Correlator scaling table.
 20   MSGSAV = MSGSUP
      MSGSUP = 31900
      CALL GETCTL (CATR, FACT2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      CSTOK = MSGTXT(1:1)
      NS = NSTOK
      LIMISJ = NS
C                                       Set AP pointers
      LROW = NY
      TOLROW = 2 * LROW
C                                       Determine no. rows in AP -
C                                       any excess over allowing 1/4
C                                       of the AP for data buffer
C                                       goes to extra rows.
      MAXREC = MIN (2048, (APMAXW/30)/(LLREC+4)) + 5
      MAXREC = MAX (15, MAXREC)
      EXCESS = APMAXW - MAXREC * (LLREC+4) - NCHAN - 222 - 200*KINT -
     *   KINT * TOLROW
      MXROGD = KINT
      IF (EXCESS.GT.0) MXROGD = MXROGD + EXCESS / TOLROW
      IFACT = 2
      LOGRID = APMAXW - (MXROGD * IFACT) * NY - 1
      CX = LOGRID - 200 * (KINT+1) - 1
      MM = KINT
      FLAG = 1
      IF ((KSTOK.EQ.3) .AND. (ICOR0.LE.0)) FLAG = -1
C                                       Set other AP values.
      LROW = NY
C                                       model only
      IF (FACGRD(2).EQ.0.0) THEN
         UUU = 0.0
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
C                                       Scale FACT2 by -FACGRD
C                                       data - model
      ELSE
         UUU = 1.0
         FACT2(1) = -FACT2(1) * FACGRD(1)
         FACT2(2) = -FACT2(2) * FACGRD(1)
         END IF
C                                       If FFT scale by 0.5
      IF (DOFFT) THEN
         FACT2(1) = FACT2(1) * 0.5
         FACT2(2) = FACT2(2) * 0.5
         END IF
C                                       Prepare for BS loop
      LOCHAN = 1
      NCHGRD = 1
C                                       Loop over frequency channel
C                                       for bandwidth synthesis.
 30      CONTINUE
C                                       Open files. Use UVINIT and
C                                       UVDISK to read and write the
C                                       visibility file,
C                                       APIO to read the grid file.
C                                       Set input file name.
         IF (IDISK.LE.0) THEN
            VOL = SCRVOL(ICNOSC)
            CALL ZPHFIL ('SC', VOL, SCRCNO(ICNOSC), 1, NAME, IRET)
         ELSE
            VOL = IDISK
            CALL ZPHFIL ('UV', VOL, ICNOSC, 1, NAME, IRET)
            END IF
         CALL ZOPEN (LUNS(2), INDS2, VOL, NAME, T, F, T, IRET)
         IF (IRET.LE.0) GO TO 40
            WRITE (MSGTXT,1030) IRET
            GO TO 995
C                                       Reset input file to output file.
 40      IDISK = DISKO
         ICNOSC = CNOSCO
C                                       Init UV file read.
         CALL UVINIT ('READ', LUNS(2), INDS2, NVIS, VO, LREC, ILENBU,
     *      JBUFSZ, BUFF1, BO, BIND2, IRET)
         IF (IRET.EQ.0) GO TO 50
            WRITE (MSGTXT,1040) IRET
            GO TO 995
C                                       UV file for write
C                                       Set output file name.
 50      IF (DISKO.GT.0) THEN
            VOL = DISKO
            CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
         ELSE
            VOL = SCRVOL(CNOSCO)
            CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
            END IF
         CALL ZOPEN (LUNS(3), INDS3, VOL, NAME, T, F, T, IRET)
         IF (IRET.LE.0) GO TO 60
            WRITE (MSGTXT,1030) IRET
            GO TO 995
C                                       Init UV file write.
 60      CALL UVINIT ('WRIT', LUNS(3), INDS3, NVIS, VO, LREC, ILENBU,
     *      JBUFSZ, BUFF3, BO, BIND3, IRET)
         NIOUT = ILENBU
         IF (IRET.EQ.0) GO TO 70
            WRITE (MSGTXT,1040) IRET
            GO TO 995
C                                       Read first visibility record
 70      CALL UVDISK ('READ', LUNS(2), INDS2, BUFF1, NIO, BIND2, IRET)
         IF (IRET.EQ.0) GO TO 80
            WRITE (MSGTXT,1100) IRET
            GO TO 995
 80      INPTR = BIND2
         INCNT = 1
         VISOUT = 0
         VISRED = 0
C                                       Determine first U (in cells)
         FRSTU = ABS (BUFF1(BIND2)) * ZSCLU
         FREQHI = MAX( FREQG(CHANEL+NCHAN-1), FREQG(CHANEL))
C                                       Set NCHGRD (no. ch to grid)
         NCHGRD = 1
C                                       Reset for bandwidth synthesis
         FREQLO = FREQG(CHANEL+LOCHAN-1)
         FREQHI = FREQLO
         DO 90 II = LOCHAN,NCHAN
            IF (FREQG(CHANEL+LOCHAN-1).LT.FREQG(CHANEL+II-1)) THEN
               IU1 = FRSTU * FREQG(CHANEL+LOCHAN-1) / FREQ
               IU2 = FRSTU * FREQG(CHANEL+II-1) / FREQ + 1.0
            ELSE
               IU2 = FRSTU * FREQG(CHANEL+LOCHAN-1) / FREQ + 1.0
               IU1 = FRSTU * FREQG(CHANEL+II-1) / FREQ
               END IF
            IF (IU2-IU1+KINT+1.GT.MXROGD) GO TO 100
               NCHGRD = (II - LOCHAN) + 1
               INDEX = CHANEL + II - 1
               FREQLO = MIN (FREQLO, FREQG(INDEX))
               FREQHI = MAX (FREQHI, FREQG(INDEX))
 90         CONTINUE
 100     LIMIT = LOCHAN + 1
         DO 110 II = LIMIT,NCHAN
C                                       Frequency scaling table.
C                                       Use BUFF3.
            INDEX = CHANEL + II - 1
            BUFF3(II-LOCHAN) = (FREQG(INDEX) / FREQG(INDEX-1)) - 1.0D0
 110        CONTINUE
C                                       Determine no. rows needed.
         IU1 = FREQLO * FRSTU / FREQ
         IU2 = FREQHI * FRSTU / FREQ + 1.0
         NROGRD = KINT + IU2 - IU1 + 1
         HICHAN = LOCHAN + NCHGRD - 1
         INDEX = CHANEL + LOCHAN - 1
         NF = CHANEL + HICHAN - 1
         IF (MFIELD.LE.1) THEN
            WRITE (MSGTXT,1110) CSTOK, INDEX, NF
         ELSE
            WRITE (MSGTXT,1111) IFIELD, CSTOK, INDEX, NF
            END IF
         CALL MSGWRT (2)
         NF = NCHGRD
C                                       Reset GRID to top of AP
         GRID = APMAXW - (NROGRD * IFACT) * NY - 1
C                                       Set values for shifting GRID
C                                       at end of each row.
         END1 = LOGRID + (NROGRD-1) * NY * IFACT - 1
         END2 = APMAXW - 2
         NMOV = (IFACT * NY) * (NROGRD-1)
C                                       Determine where data starts.
         FLIST(10) = ((FRSTU*FREQHI/FREQ + 0.5) - (KINT/2)) + NROGRD
         FLIST(10) = MIN (FLIST(10), NX2)
C                                       Get frequency scaling factors.
         FFRAC = (FREQLO / FREQ) - 1.0D0
         GFACT = 1.0D0 / ((1.0D0 + FFRAC) * ZSCLU)
         GFAC2 = (1.0D0 + FFRAC) * ZSCLU
         FFRAC = (FREQHI / FREQ) - 1.0D0
         SCALU = FFRAC * ZSCLU + ZSCLU
         SCALV = FFRAC * ZSCLV + ZSCLV
C                                       Set limits on u,v - don't get
C                                       within MINT/2+1 of edge.
         UUMAX = ABS (((NX/2) - (MINT/2+1)) / SCALU)
         VVMAX = ABS (((NY/2) - (MINT/2+1)) / SCALV)
C                                       Setup for AP griding
C                                       Set constants in AP.
         FFRAC = (FREQG(LOCHAN+CHANEL-1) / FREQ) - 1.0D0
         XTEMP(1) = (ZSCLU + FFRAC * ZSCLU)
         XTEMP(2) = (ZSCLV + FFRAC * ZSCLV)
         DDX = DXCG(IFIELD)
         DDY = DYCG(IFIELD)
         XTEMP(3) = DDX / ZSCLU
         XTEMP(4) = DDY / ZSCLV
         XTEMP(5) = DZCG(IFIELD) * FFRAC + DZCG(IFIELD)
         XTEMP(6) = 0.0
         XTEMP(7) = 0.0
         IXTEMP(1) = IU
         IXTEMP(2) = GRID
         IXTEMP(3) = CX
         CALL QPUT (XTEMP, 0, 7, 2)
         CALL QPUT (IXTEMP, 7, 3, 1)
C                                       Correlator scaling table.
         ITEMP = 16
         CALL QPUT (FACT2, ITEMP, NS, 2)
         CALL QPUT (UUU, 15, 1, 2)
C                                       Frequency scaling table.
         ITEMP = 20
         CALL QPUT (BUFF3, ITEMP, NF, 2)
C                                       Set interpolation tables
         CALL INTPFN (CX, KINT, BUFF3, JBUFSZ, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
C                                       Save space for four word work
C                                       array.
         MAXREC = (CX - 20 + NS + NF) / (LREC+4) - 5
         IF (MAXREC.GT.MAXCNJ) MAXREC = MAXCNJ
C                                       Be sure MAXREC.GT.10
         IF (MAXREC.GT.10) GO TO 130
            XMAX = - MAXREC * REAL (LREC)
            WRITE (MSGTXT,1090) XMAX
            IRET = 1
            GO TO 995
C                                       More AP pointers.
 130     WORK = 20 + NF
         UV = WORK + 4 + (MAXREC * 4)
C                                       Compute offset from beginning of
C                                       record to first correlator.
C                                       Offset = NRPARM + LVIS * CHANEL
C                                       LVIS = no. words per correlator
         LVIS = MIN (INCS, INCF)
         IF ((INCIF.GT.0) .AND. (INCIF.LT.LVIS)) LVIS = INCIF
C                                       Correction for stokes.
         VISOFF = NRPARM + ((CHANEL-1) + (LOCHAN-1))*INCF + VOFF
         VIS = UV + VISOFF
C                                       Put first NROGRD rows in AP.
         DO 150 IM = 1,NROGRD
            INDEX = NROGRD - IM
            INDEX = GRID + INDEX * TOLROW
            CALL APIO ('READ', FLIST, INDEX, BUFF2, IRET)
            IF (IRET.EQ.0) GO TO 150
               WRITE (MSGTXT,1230) IRET, IM
               GO TO 995
 150        CONTINUE
C
C                                       Loop through grid
         KLIM = FLIST(10) - NROGRD + KINT/2 + 1
         DO 400 I = 1,KLIM
            IU = KLIM - I
            UMIN = (IU - 0.5) * GFACT
            UMIN = (IU - 0.4975)
            IDATA = UV
            CNT = 0
            INDCNJ = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
  160       ENDROW = T
            NPOINT = 0
C                                       Check if all data read.
            IF (NIO.LE.0) GO TO 220
C                                       See if need a new input buffer
            IF (INCNT.GT.NIO) GO TO 200
C                                       Make sure there is some data on
C                                       this row.
            UTEMP = ABS (BUFF1(INPTR)) * GFAC2
            IF (UTEMP.LE.UMIN) GO TO 220
C                                       Check if end of row occures in
C                                       this record.
            LIM = INCNT + MAXREC - CNT - 1
            IF (LIM.GT.NIO) LIM = NIO
            JNPTR = INPTR
C                                       Check for end of row and look
C                                       for data on wrong plane half.
            DO 180 II = INCNT,LIM
C                                       Check that data inside grid.
               IF ((ABS (BUFF1(JNPTR))  .GT.UUMAX) .OR.
     *             (ABS (BUFF1(JNPTR+1)).GT.VVMAX)) GO TO 980
               INDCNJ = INDCNJ + 1
C                                       Check if datum in current row.
               UTEMP = ABS (BUFF1(JNPTR)) * GFAC2
               IF (UTEMP.LE.UMIN) GO TO 190
C                                       Check if U neg.; if so then flip
C                                       data to other half plane.
               IF (BUFF1(JNPTR).LT.0.0) THEN
C                                       Check max. no. in table.
                  IF (NCONJ.GE.MAXCNJ) GO TO 185
C                                       Enter in CNJPTR table.
                  NCONJ = NCONJ + 1
                  CNJPTR(NCONJ) = INDCNJ
C                                       Flip U, V, W, conjugate data.
                  CALL UVCONJ( VISOFF, INCF, NCHGRD, LIMISJ, INCS,
     *               BUFF1(JNPTR))
C                                       end if U < 0, conjugate
                  END IF
               NPOINT = NPOINT + 1
               VISRED = VISRED + 1
               JNPTR = JNPTR + LREC
C                                       end for all vis recs read
 180           CONTINUE
C                                       Rest of record is on same row.
 185        ENDROW = F
 190        CONTINUE
            CNT = CNT + NPOINT
C                                       Load into AP.
            CALL QWR
            ITEMP = NPOINT * LLREC
            CALL QPUT (BUFF1(INPTR), IDATA, ITEMP, 2)
C                                       Low row to AP
            IXTEMP(1) = IU
            IXTEMP(2) = GRID
            CALL QPUT (IXTEMP, 7, 3, 1)
            IDATA = IDATA + ITEMP
            INCNT = INCNT + NPOINT
            INPTR = INPTR + NPOINT * LREC
C                                       Check if AP full or row finished
            IF ((ENDROW) .OR. (CNT.GE.MAXREC) .OR. (NCONJ.GE.MAXCNJ))
     *         GO TO 220
C                                       Read next record.
  200       INCNT = 1
C                                       Check if all records read.
            ENDROW = NIO.EQ.0
            IF (NIO.GT.0) THEN
               CALL UVDISK ('READ', LUNS(2), INDS2, BUFF1, NIO, BIND2,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  GO TO 995
                  END IF
               INPTR = BIND2
C                                       Loop back
               GO TO 160
               END IF
C                                       exit loop if no more data
 220        CONTINUE
            INDCNJ = 0
C                                       Interpolate data
            IF (CNT.LE.0) GO TO 250
            CALL QWAIT
            CALL QUVIN (UV, VIS, WORK, LLREC, MM, LROW, NS, IINCS,
     *         NF, IINCF, CNT, FLAG)
            CALL QWR
C                                       Read out data.
            IDATA = UV
C                                       Setup for flipping half plane
            FCONJ = 1
            INDCNJ = 0
C                                       Loop, transfering to disk
 230           NWRITE = NIOUT
               IF (NWRITE.GT.CNT) NWRITE = CNT
               ITEMP = NWRITE * LLREC
               CALL QGET (BUFF3(BIND3), IDATA, ITEMP, 2)
               IDATA = IDATA + ITEMP
               NIOUT = NWRITE
               CALL QWD
C                                       Check if any data in buffer
C                                       needs to be reflipped to
C                                       other half plane.
 234           IF ((FCONJ.LE.NCONJ) .AND.
     *             ((INDCNJ+NWRITE) .GE. CNJPTR(FCONJ))) THEN
C                                       Loop flipping as necessary.
                  JNPTR = BIND3 + (CNJPTR(FCONJ)-INDCNJ-1) * LREC
C                                       Flip U, V, W, conjugate data.
                  CALL UVCONJ( VISOFF, INCF, NCHGRD, LIMISJ, LVIS,
     *               BUFF3(JNPTR))
                  FCONJ = FCONJ + 1
C                                       Loop back until all cojugated
                  GO TO 234
C                                       end if data to conjigate back
                  END IF
               INDCNJ = INDCNJ + NWRITE
               VISOUT = VISOUT + NIOUT
C                                       Write data to disk
               CALL UVDISK ('WRIT', LUNS(3), INDS3, BUFF3, NIOUT,
     *            BIND3, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  GO TO 995
                  END IF
               CNT = CNT - NWRITE
               IF (CNT.GT.0) GO TO 230
C                                       Reset counters pointers.
            CNT = 0
            FCONJ = 1
            NCONJ = 0
            INDCNJ = 0
C                                       See if done.
            IF (NIOUT.LE.0) GO TO 530
            IDATA = UV
C                                       Check if row finished.
            IF (.NOT.ENDROW) THEN
               IF (INCNT.LE.NIO) GO TO 160
               GO TO 200
               END IF
C                                       Prepare AP for next row.
 250        IF ((GRID.EQ.LOGRID) .AND. (IU.NE.0)) THEN
               CALL QVMOV (END1, -1, END2, -1, NMOV)
               GRID = END2 - NMOV + 1
               END IF
C                                       Reset GRID
            GRID = GRID - NY * IFACT
            GRID = MAX (GRID, LOGRID)
C                                       when AP Finishes do I/O
            CALL QWAIT
C                                       prepare for next row
            JU = IU - 1
C                                       when not near end of rows
            IF (IU.GT.KINT/2) THEN
C                                       Put next row into AP.
               CALL APIO ('READ', FLIST, GRID, BUFF2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1230) IRET, I
                  GO TO 995
                  END IF
            ELSE
C                                       else near end of rows, create
C                                       U<0 rows from conj U>0 row
C                                       calc distance between rows
               INDEX = ((2*(KINT/2)) - (2*JU) + 1) * IFACT * NY - IFACT
C                                       GRID -> start JU-KINT/2 row
               INDEX = GRID + INDEX
C                                       INDEX -> start JU+KINT/2 ROW
               CALL QCVCON (INDEX, -IFACT, GRID+IFACT, IFACT, ONENY-1)
C                                       INDEX -> end JU+KINT/2 Row
               INDEX = INDEX - (IFACT*NY) + IFACT
C                                       conjugate a single pixel
               CALL QCVCON (INDEX, IFACT, GRID, IFACT, 1)
               END IF
C                                       end for all rows loop
 400        CONTINUE
C                                       Finish write.
 530     NIOUT = 0
         CALL UVDISK ('FLSH', LUNS(3), INDS3, BUFF3, NIOUT, BIND3, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1230) IRET, I
            GO TO 995
            END IF
C                                       Give up AP.
         CALL QRLSE
C                                       Close files
         CALL ZCLOSE (LUNS(2), INDS2, IRET)
         CALL ZCLOSE (LUNS(3), INDS3, IRET)
C                                       Check that written all data read
         IF (VISOUT.NE.VISRED) THEN
            WRITE (MSGTXT,1541) NVIS, VISOUT
            CALL MSGWRT (8)
            END IF
C                                       INIT grid file.
         CALL APIO ('INIT', FLIST, GRID, BUFF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1540) IRET
            GO TO 995
            END IF
C                                       End frequency (BS) loop.
         LOCHAN = LOCHAN + NCHGRD
         IF (LOCHAN.LE.NCHAN) GO TO 30
C                                       Close grid file.
      CALL APIO ('CLOS', FLIST, GRID, BUFF2, IRET)
      GO TO 999
C                                       Data outside of grid
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
      WRITE (MSGTXT,1981) UUMAX, VVMAX
      IRET = 5
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ALGSUB: TROUBLE OPENING UV DATA FILE, IER=',I5)
 1040 FORMAT ('ALGSUB: ERROR',I5,' INITIALIZING FILE')
 1090 FORMAT ('ALGSUB: ',F8.0,' TOO FEW AP WORDS AVAILABLE')
 1100 FORMAT ('ALGSUB: READ ERROR IN VISIBILITY FILE, IER=',I5)
 1110 FORMAT ('ALGSUB: ',A1,'pol gridded model subtraction, chans',I5,
     *   ' through',I5)
 1111 FORMAT ('ALGSUB field',I5,1X,A1,'pol gridded model subtraction',
     *   ' chns',I5,'-',I5)
 1230 FORMAT ('ALGSUB: ERROR',I5,' READING GRID ROW ',I5)
 1540 FORMAT ('ALGSUB: TROUBLE INIT GRID FILE, IER=',I5)
 1541 FORMAT ('ALGSUB: WARNING: MISPLACED DATA, NVIS = ',I8,
     *   ' WRITTEN =',I8)
 1980 FORMAT ('ALGSUB: ERROR: DATA OUTSIDE OF UV GRID USED:')
 1981 FORMAT ('        UMAX = ',1PE12.5,' VMAX = ',1PE12.5,
     *   ' WAVELENGTH')
      END
