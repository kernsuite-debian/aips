      SUBROUTINE VSCDFT (CHANEL, NCHAN, DISKI, CNOSCI, DISKO, CNOSCO,
     *   IFIELD, DOSUM, DOMSG, MAXBWC, BWCOR, FRQCOR, CATR, JBUFSZ,
     *   BUFF1, BUFF2, IBUFF, IRET)
C-----------------------------------------------------------------------
C! Compute DFT of model with corrections and add to uv data.
C# AP-appl UV Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2003, 2006-2009, 2013-2014
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
C   VSCDFT DFTs clean components correcting for "W"  then adds the model
C   to a set of uv data.  Only point models are processed.
C   Can also optionally make minor corrections for bandwidth smearing
C   and correct the values of u,v, and w for errors in the reference
C   frequency.
C     WARNING: changed in 31DEC09 to leave out the full 3D correction
C     for W in DO3DIM false since it is doing a "3D" correction in the
C     U-V plane instead.
C   Inputs:
C      CHANEL   I        Frequency channel
C      NCHAN    I        Number of frequency channels.
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      IFIELD   I        Field to do (0 -> all)
C      DOSUM    L        If true sum the flux in each field
C      DOMSG    L        If true give percent done messages.
C      MAXBWC   R        Maximum bandwidth smearing correction in %
C      BWCOR    R        Bandwidth correction factor. 0 => no corr.
C                        Multiplied times nominal (CATBLK) bandwidth.
C      FRQCOR   R        Ref. frequency correction factor. 0 => no corr.
C      JBUFSZ   I        Size of BUFF1,2, IBUFF in AIPS bytes, each
C                        must be at least 4096 words.
C   Inputs: from commons
C      MFIELD   I        Number of fields
C      NCLNG    I(16)    Number of components per field.
C      NSUBG    I(16)    The next component to subtract.
C      CCDISK   I(16)    Disk numbers of the clean images.
C      CCCNO    I(16)    Catalog slot numbers of clean images.
C      CCVER    I(*)     CC file version number for each field.
C      NGRDAT   L        If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C      LREC     I        Length in words of vis record.
C      NVIS     I        Number of vis. records
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop if abs(flux) < LIMFLX
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C      PARMOD   R(6)     Model parameters for non point models; used
C                        only if DOPTMD is true.
C                        1=> model type, 0=point,
C      RA       D        RA of uv data tangent point. (deg.)
C      DEC      D        Declination of uv data tangent point.
C   In/out:
C      CNOSCO   I        IN: output file catalog slot number or /CFILES/
C                        scratch file number. Will create a scratch file
C                        if CNOSCO and DISKO .le. 0.
C                        Out: file /CFILES/ number if created.
C      CATR     R(256)   Output UV data catalog header record; frequency
C                        may be updated.
C   Output:
C      BUFF1    R(*)     I/O buffers.
C      BUFF2    R(*)     I/O buffers.
C      IBUFF    I(*)     I/O Buffer.
C      IRET     I        Return code, 0 => ok, otherwise not.
C                            9 => Buffers too small to load AP.
C-----------------------------------------------------------------------
      INTEGER   CHANEL, NCHAN, DISKI, CNOSCI, DISKO, CNOSCO, IFIELD,
     *   JBUFSZ, IBUFF(*), IRET
      LOGICAL   DOSUM, DOMSG
      REAL      MAXBWC, BWCOR, FRQCOR, BUFF1(*), BUFF2(*), CATR(256)
C
      CHARACTER KEYS(7)*8, NAME*48, ERRTXT*40
      INTEGER   JCOMP, JNCOMP, CCOUNT, XNCOMP, MXCMP, CURCMP, JT, IADR,
     *   NCOMP, J, MCOMP, VO, BO, ISIZE, INIO, MMCOMP, NNCOR, IDATA, UV,
     *   LLREC, IAPBUF, IAPCC, IAPCT, LMCOMP, IAPTMP, VIS, WRK, LLNMOD,
     *   MCHAN, JNCS, JNCF, KAP, SFLAG, APSIZ, MXCC, INIO2, LUNC, VOL,
     *   INDEX, ITYPE, NIOUT, KBIND, LENBU, LENMOD, JLREC, JNREC, FINDI,
     *   FINDO, I, LUNI, LUNO, ITIME(3), IBIND, LFIELD, NKEY, APFSCL,
     *   KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, MAJKOL, MINKOL, PAKOL,
     *   IPCLST, IPCDNE, NCALL, NTIMES, LRPARM, APBWC, LF1, LF2, NEED,
     *   MSGSAV, KLNBL2(256)
      REAL      XXOFF, YYOFF, ZZOFF, RECORD(20), FACT2(4), BWPRM(5),
     *   BWFAC, FQFAC, X, Y, XYZ(3), XP(3), UMAT(3,3), PMAT(3,3)
      DOUBLE PRECISION XTLST, PCTOT, PCLST, FREQC, FRQADD, RAPNT,
     *   DECPNT, XRA, XDEC, XPR, YPR, CONST, CONST2
      LOGICAL   T, F
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   DONE(MAXFLD), ONZE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      PARAMETER (CONST = DG2RAD * TWOPI)
      SAVE ONZE
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5) ,MAJKOL),               (KOLS(6), MINKOL),
     *   (KOLS(7), PAKOL)
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ',
     *   'TYPE OBJ', 'MAJOR AX', 'MINOR AX', 'POSANGLE'/
      DATA VO, BO, MXCC /0, 1, 1024/
      DATA LUNI, LUNO, LUNC /22,23,29/
      DATA T, F /.TRUE.,.FALSE./, ONZE/.FALSE./
C-----------------------------------------------------------------------
C                                       CONST2 converts FWHM(deg) to
C                                       coefficients for u*u, v*v, u*v
      CONST2 = DG2RAD * (PI / 1.17741022D0) * SQRT (0.5D0)
C                                       Tell kind of operation.
      MSGTXT = 'VSCDFT: Begin DFT model calculation with corrections'
      CALL MSGWRT (2)
C                                       Tell about bandwidth smearing
C                                       correction.
      IF (MAXBWC.GT.1.0E-20) THEN
         WRITE (MSGTXT,1000) MAXBWC
         CALL MSGWRT (4)
         END IF
C                                       Tell about bandwidth correction.
      BWFAC = 1.0
      IF (BWCOR.GT.1.0E-20) THEN
         WRITE (MSGTXT,1001) BWCOR
         CALL MSGWRT (4)
         BWFAC = BWCOR
         END IF
C                                       Tell about ref. freq. correction.
      FQFAC = 1.0
      FREQC = FREQ
      FRQADD = 0.0D0
      IF (FRQCOR.GT.1.0E-20) THEN
         WRITE (MSGTXT,1002) FRQCOR
         CALL MSGWRT (4)
         FQFAC = FRQCOR
         FREQC = FREQ * FQFAC
         FRQADD = FREQC - FREQ
C                                       Update catalog frequency
         IADR = (KDCRV+JLOCF-1) * NWDPDP + 1
         CALL COPY (NWDPDP, FREQC, CATR(IADR))
         END IF
      MCHAN = NCHAN
      NNCOR = 1
C                                       Get un-compressed UV increments
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, LRPARM,
     *   LLREC)
      LFIELD = 0
      CCOUNT = 0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
C                                       Decide component type.
C                                       From model passed
      IF (DOPTMD) THEN
C                                       From CC table, field 1
      ELSE
         LFIELD = LF1
C                                       If NGRDAT read CLEAN CATBLK.
         IF (NGRDAT) THEN
            ERRTXT = 'READING CLEAN CATBLK'
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBLK,
     *         'REST', IBUFF(2049), IRET)
            IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
C                                       Get field info. if nec.
         ELSE
            CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         JNREC = 1
         JLREC = 0
         NKEY = 0
C                                       Make sure this header applies
C                                       to this image
         CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBL2,
     *      'REST', BUFF1, IRET)
         IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
            ERRTXT =  'READ HEADER WITH CATIO'
            GO TO 990
            END IF
C                                       Tabini can update header on disk
         ERRTXT = 'OPENING CLEAN COMPS FILE'
         CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *      ABS(CCVER(LFIELD)), KLNBL2, LUNC, NKEY, JNREC, JLREC,
     *      BUFF1, IBUFF, IRET)
         IF (IRET.GT.1) GO TO 990
C                                       More complex models
C                                       Find columns (physical)
         IF (JLREC.GT.3) THEN
            NKEY = JLREC
            IF (JLREC.GT.7) NKEY = 7
            ERRTXT = 'FINDING REQUIRED CC COLUMNS'
            CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
C                                       Close CLEAN components file.
         CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
         END IF
C                                       Determine no. CC to sub.
      XNCOMP = 0
      DO 10 LFIELD = LF1,LF2
         DONE(LFIELD) = F
         XNCOMP = XNCOMP + NCLNG(LFIELD) - NSUBG(LFIELD) + 1
 10      CONTINUE
C                                       Check for point model.
      IF (DOPTMD) XNCOMP = 1
C                                       Tell model type once
      IF (.NOT. ONZE) THEN
C                                       Tell model type
         MSGTXT = 'VSCDFT: Model components of type Point'
         CALL MSGWRT (2)
         ONZE = .NOT. ONZE
C                                       Check Buffer size
         IF (XNCOMP.GT.10 .AND. (JBUFSZ/2 .LT. 4096)) THEN
            MSGTXT = 'VSCDFT: SCRATCH BUFFER TO SMALL FOR CCs!'
            CALL MSGWRT (8)
            IRET = 9
            GO TO 999
            END IF
         END IF
C                                       Set model length
      LENMOD = 9
C                                       Determine size of uv I/O and
C                                       the number of CC that will fit.
      LENBU = ((JBUFSZ-2*NBPS) / 2) / (LLREC*2)
C                                       How much data fits in AP?
      JT = 100 + LENBU * LLREC
      IF (MXCC.GT.JT) JT = MXCC
      NEED = JT + NCHAN + (XNCOMP+2)*LENMOD
      NEED = NEED / 1024
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = JT + NCHAN + (XNCOMP/10+2)*LENMOD
         NEED = NEED / 1024
         NEED = MIN (32*1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
             IRET = 8
             MSGTXT = 'VSCDFT CANNOT GET NEEDED MEMORY'
             GO TO 995
             END IF
          END IF
      APSIZ = PSAPNW * 1024
      CALL QRLSE
      MXCMP = (APSIZ - JT - NCHAN)  / LENMOD
      MXCMP = MXCMP - 2
      CALL RFILL (20, 0.0, RECORD)
C                                       Set AP pointers.
C                                       UV=UV pointer, VIS=vis pointer
C                                       IAPCC=CLEAN components pointer.
      IDATA = 15 + NCHAN
      UV = IDATA + ILOCU
      APBWC = IDATA + LENBU * LLREC
      APFSCL = APBWC + 5
      IAPCC = APFSCL + 2
      LLNMOD = LENMOD
      LFIELD = LF1 - 1
C                                       Compute number of passes.
      NTIMES = (1.0 * XNCOMP) / MXCMP + 0.99999
      NTIMES = MAX (NTIMES, 1)
C                                       Open uv files.
C                                       Set input file name.
      IF (DISKI.LE.0) THEN
         VOL = SCRVOL(CNOSCI)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1, NAME, IRET)
      ELSE
         VOL = DISKI
         CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
         END IF
C                                       Open input file.
      ERRTXT = 'OPEN-FOR-READ VIS FILE'
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create scratch file if necessary
      IF ((DISKO.LE.0) .AND. (CNOSCO.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         ERRTXT = 'CREATING SCRATCH FILE'
         CALL SCREAT (ISIZE, BUFF2, IRET)
         CNOSCO = NSCR
         IF (IRET.GT.0) THEN
            IF (IRET.EQ.1) ERRTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
C                                       End if creating scratch file
         END IF
C                                       Open vis file for write.
      IF (DISKO.LE.0) THEN
         VOL = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
      ELSE
         VOL = DISKO
         CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
         END IF
      ERRTXT = 'OPEN-FOR-WRITE VIS FILE'
      CALL ZOPEN (LUNO, FINDO, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop, adding max component
C                                       in AP each pass
      DO 500 NCALL = 1,NTIMES
C                                       Setup for % done messages.
         PCTOT = NVIS
         PCLST = PCTOT * (NCALL - 1)
         IPCLST = (100. / NTIMES ) * (NCALL - 1)
C                                       Set AP loc for next CC load
         IAPCT = IAPCC
C                                       Determine. no. this pass.
         MMCOMP = 0
         NCOMP = MIN( MXCMP, XNCOMP)
C                                       Grab AP.
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'VSCDFT: BIZARRE FAILURE TO GET NEEDED AP MEMORY'
            IRET = 10
            GO TO 995
            END IF
C                                       Initialize REAL time clock for
C                                       AP roller.
         CALL ZTIME (ITIME)
         XTLST = (ITIME(1) * 60.00) + ITIME(2) + (ITIME(3) / 60.0)
C                                       If Not single component model.
         IF (.NOT. DOPTMD) THEN
C                                       Find next FIELD.
C                                       Loop back here for next field.
 70         LFIELD = LFIELD + 1
C                                       See if done.
            IF (LFIELD.GT.LF2) GO TO 150
            IF (DONE(LFIELD)) GO TO 70
C                                       See if there are CC's.
            IF ((NSUBG(LFIELD).GT.NCLNG(LFIELD))) GO TO 70
C                                       If NGRDAT read CLEAN CATBLK.
            IF (NGRDAT) THEN
               ERRTXT = 'READING CLEAN CATBLK'
               CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *            KLNBLK, 'REST', IBUFF(2049), IRET)
               IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
C                                       Get field info. if nec.
            ELSE
               CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Set field center offsets.
            XXOFF = DXCG(LFIELD) * CCROT + DYCG(LFIELD) * SSROT
            YYOFF = DYCG(LFIELD) * CCROT - DXCG(LFIELD) * SSROT
            ZZOFF = DZCG(LFIELD)
            CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD),
     *         MAPROT, XRA, XDEC)
            IF (DO3DIM) THEN
               CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
            ELSE
               CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
               END IF
C                                       Load CLEAN components into AP.
C                                       Open components file.
            JNREC = 1
            JLREC = 0
            NKEY = 0
            ERRTXT = 'OPENING CLEAN COMPS FILE'
            CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *         ABS(CCVER(LFIELD)), KLNBLK, LUNC, NKEY, JNREC, JLREC,
     *         BUFF1, IBUFF, IRET)
            IF (IRET.GT.1) GO TO 990
            JCOMP = NSUBG(LFIELD)
C                                       Find columns (physical)
            NKEY = JLREC
            TYPKOL = 7
            IF (JLREC.GT.7) NKEY = 7
            ERRTXT = 'FINDING REQUIRED CC COLUMNS'
            CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
            IF (IRET.GT.0) GO TO 990
C                                       Make sure that there are some
            IF (IBUFF(5).LE.0) GO TO 140
            IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
C                                       Loop loading components.
            IAPBUF = 10
            CURCMP = MMCOMP + 1
C                                       Check next component
            IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 140
            DO 130 J = CURCMP,NCOMP,MXCC
               JT = J - 1
               JNCOMP = 0
               MCOMP = NCOMP - J + 1
               IF (MCOMP.GT.MXCC) MCOMP = MXCC
               IF (MCOMP.GT.(NCLNG(LFIELD)-NSUBG(LFIELD)+1))
     *            MCOMP = NCLNG(LFIELD) - NSUBG(LFIELD) + 1
               DO 110 I = 1,MCOMP
C                                       Check if finished field
                  IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 120
                  CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1090) IRET, JCOMP
                     GO TO 995
                     END IF
C                                        Check that point comp.
                  JCOMP = JCOMP + 1
                  JT = JT + 1
                  ITYPE = RECORD(TYPKOL) + 0.5
                  IF ((ITYPE.EQ.0) .AND. (IRET.EQ.0)) THEN
C                                       Check negative component limit
                     DONE(LFIELD) = (NONEG.AND.(RECORD(FLXKOL).LE.0.0))
     *                  .OR. (ABS(RECORD(FLXKOL)).GT.LIMFLX)
                     IF (DONE(LFIELD)) THEN
                        NCLNG(LFIELD) = JCOMP - 1
                        GO TO 120
                        END IF
C                                       If req. sum flux
                     IF (DOSUM) THEN
                        FLUXG(LFIELD) = FLUXG(LFIELD) + RECORD(FLXKOL)
                        TFLUXG = TFLUXG + RECORD(FLXKOL)
                        END IF
                     JNCOMP = JNCOMP + 1
                     CCOUNT = CCOUNT + 1
                     XP(1) = (RECORD(RAKOL) + XPOFF(LFIELD)) * CONST
                     XP(2) = (RECORD(DECKOL) + YPOFF(LFIELD)) * CONST
                     XP(3) = 0.0
                     CALL PRJMUL (2, XP, UMAT, XYZ)
C                                       Full 3-D correction.
C                    XPR = (XYZ(1) + XXOFF) / TWOPI
C                    YPR = (XYZ(2) + YYOFF) / TWOPI
C                    XYZ(3) = -(SQRT (1.0D0-XPR*XPR-YPR*YPR) - 1.0D0)
C                    XYZ(3) = XYZ(3) * TWOPI
                     BUFF1(JNCOMP) = XYZ(1) + XXOFF
                     BUFF1(1024+JNCOMP) = XYZ(2) + YYOFF
                     BUFF1(2048+JNCOMP) = XYZ(3) + ZZOFF
                     BUFF1(3072+JNCOMP) = RECORD(FLXKOL)
                     END IF
 110              CONTINUE
C                                       Load components
 120           IF (JNCOMP.GT.0) THEN
                  LMCOMP = JNCOMP
                  MMCOMP = MMCOMP + LMCOMP
C                                       Load into AP
                  IAPBUF = 10
C                                       x component
                  CALL QPUT (BUFF1, IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 6
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       y component
                  CALL QPUT (BUFF1(1025), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 7
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       z component
                  CALL QPUT (BUFF1(2049), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 8
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Flux density
                  CALL QPUT (BUFF1(3073), IAPBUF, LMCOMP, 2)
                  CALL QWD
C                                       Move flux to location.
                  CALL QVMOV (IAPBUF, 1, IAPCT, LLNMOD, LMCOMP)
                  CALL QWR
                  IAPCT = IAPCT + (LLNMOD * LMCOMP)
                  END IF
C                                       Check if finished field.
               IF ((JCOMP.GT.NCLNG(LFIELD)) .OR. DONE(LFIELD))
     *            GO TO 140
 130           CONTINUE
C                                       Close CLEAN components file.
 140        CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
C                                       Update field sub. count.
            NSUBG(LFIELD) = JCOMP
C                                       Check if need another field.
            IF (JT.LT.NCOMP) GO TO 70
C                                      Check no. comps.
 150        IF ((MMCOMP.EQ.0) .AND. (NCALL.EQ.1)) THEN
               ERRTXT = 'NO POINT COMPONENTS FOUND'
               IRET = 1
               GO TO 990
               END IF
C                                       No comps on later pass is OK
            IF (MMCOMP.LE.0) GO TO 510
C                                       Load correlator factors
            CALL GETCTL (CATR, FACT2, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Else, single component model.
         ELSE
            CALL RFILL (LENMOD, 0.0, BUFF1)
            BUFF1(7) = PTRAOF * CONST / 3600.D0
            BUFF1(8) = PTDCOF * CONST / 3600.D0
C                                       single comp uses W term
            XPR = PTRAOF / 206264.81D0
            YPR = PTDCOF / 206264.81D0
            BUFF1(9) = -(SQRT (1.0D0 - XPR*XPR - YPR*YPR) - 1.0D0) *
     *         206264.81D0 * CONST / 3600.D0
C                                       Point
            BUFF1(1) = PTFLX
            MMCOMP = 1
            CALL QPUT (BUFF1, IAPCC, LLNMOD, 2)
            IAPCT = IAPCC + LLNMOD
C                                       Set Stokes' for point model
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            NSTOK = 1
            IF ((CATR(KRCIC+JLOCS).LT.0.0) .AND. (NCOR.GE.2)) NSTOK = 2
            IF ((NSTOK.EQ.2) .AND. (ABS (ICOR0).EQ.2)) NSTOK = 1
C                                       End if not single comp. model
            END IF
C                                       Bandwidth smearing correction
C                                       terms
         IF (MAXBWC.GT.1.0E-20) THEN
C                                       Fractional bandwidth
            BWPRM(1) = BWFAC * CATR(KRCIC+JLOCF) / FREQC
C                                       Posn. correction from obs. posn.
C                                       to tangent point.
            INDEX = ((KDORA - 1) * NWDPDP) + 1
            CALL RCOPY (NWDPDP, CATR(INDEX), RAPNT)
            INDEX = ((KDODE - 1) * NWDPDP) + 1
            CALL RCOPY (NWDPDP, CATR(INDEX), DECPNT)
C                                       If no pointing position use
C                                       tangent point.
            IF ((ABS (RAPNT).LE.1.0D-10) .AND. (ABS
     *         (DECPNT).LE.1.0D-10)) THEN
               RAPNT = RA
               DECPNT = DEC
               END IF
            X = SIN (DG2RAD * (RAPNT - RA)) * COS (DECPNT * DG2RAD)
            Y = ((COS (DEC * DG2RAD) * SIN (DECPNT * DG2RAD) -
     *         SIN (DEC * DG2RAD) * COS (DECPNT * DG2RAD) *
     *         COS ((RAPNT - RA) * DG2RAD)))
            BWPRM(2) = -TWOPI * X
            BWPRM(3) = -TWOPI * Y
            BWPRM(4) = -TWOPI * (SQRT (1.0D0 - X*X - Y*Y) - 1.0D0)
C                                       Max. correction
            BWPRM(5) = 1.0 + 0.01 * MAXBWC
         ELSE
            BWPRM(1) = 0.0
            BWPRM(2) = 0.0
            BWPRM(3) = 0.0
            BWPRM(4) = 0.0
            BWPRM(5) = 0.0
            END IF
         CALL QPUT (BWPRM, APBWC, 5, 2)
C                                       Frequency scaling factor
         CALL QPUT (FQFAC, APFSCL, 1, 2)
C                                       Correct for FACGRD
         NNCOR = NSTOK
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
         WRK = MCHAN + 2
         CALL QPUT (FACT2, WRK, NNCOR, 2)
         SFLAG = 1
C                                       Set flag for UPOL and RL,LR data
         IF ((KSTOK.EQ.3) .AND. (ICOR0.LT.0)) SFLAG = -1
C                                       Set vis pointer
         VIS = IDATA + LRPARM + (CHANEL-1) * JNCF + (VOFF/INCS)*JNCS
C                                       Fill frequency table
         BUFF2(1) = ((FREQG(CHANEL) + FRQADD) / FREQC) - 1.0D0
         IF (NCHAN.GT.1) THEN
            DO 175 I = 2,NCHAN
               INDEX = CHANEL + I - 1
               BUFF2(I) = ((FREQG(INDEX)+FRQADD) /
     *            (FREQC)) - 1.0D0
 175           CONTINUE
            END IF
         CALL QWD
         CALL QPUT (BUFF2, 2, MCHAN, 2)
         CALL QWAIT
C                                       Init for read & write
C                                       visibility file
C                                       Init vis file for write
         ERRTXT = 'INIT-FOR-READ VIS FILE'
         CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF1, BO, IBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Init vis file for read.
         ERRTXT = 'INIT-FOR-WRITE VIS FILE'
         CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF2, BO, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Subtract model from vis data.
C                                       Loop:  Read vis. record.
 200        CONTINUE
            ERRTXT = 'READING VIS FILE'
            CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO2, IBIND, IRET)
            INIO = INIO2
            IF (IRET.NE.0) GO TO 990
C                                       Exit if no more data
            IF (INIO.LE.0) GO TO 300
C                                       Uncompress Vis and put in AP
            CALL BUFPUT (INIO, NRPARM, LREC, LLREC, IDATA, BUFF1(IBIND),
     *           IBUFF(1))
C                                       Point
            CALL QPTADC (IAPCC, UV, VIS, APBWC, LLREC, JNCF, JNCS,
     *         MMCOMP, INIO, MCHAN, NNCOR, SFLAG)
C                                       Frequency scaling of u, v, w
            IF (FRQCOR.GT.1.0E-20) THEN
               CALL QVSMUL (UV, LLREC, APFSCL, UV, LLREC, INIO)
               CALL QVSMUL (UV+1, LLREC, APFSCL, UV+1, LLREC, INIO)
               CALL QVSMUL (UV+2, LLREC, APFSCL, UV+2, LLREC, INIO)
               END IF
C                                       Get UVs from AP (maybe pack UV)
            CALL BUFGET (INIO, NRPARM, LREC, LLREC, IDATA, BUFF2(KBIND),
     *         IBUFF(1))
C                                       Write vis record.
            NIOUT = INIO
            ERRTXT = 'WRITING VIS FILE'
            CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Check if time for % done
C                                       message.
            PCLST = PCLST + INIO
            IPCDNE = (100.0 / NTIMES) * (PCLST / PCTOT) + 0.5
            IPCDNE = IPCDNE - MOD (IPCDNE, 10)
C                                       Write % done message.
            IF ((IPCDNE.GT.IPCLST) .AND. (DOMSG)) THEN
               WRITE (MSGTXT,1240) IPCDNE
               CALL MSGWRT (2)
               IPCLST = IPCDNE
               END IF
C                                       Check if time for AP roller
            CALL QROLL (IAPCT, IBUFF, JBUFSZ, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 200
C                                       Finish write
 300     CALL QRLSE
         NIOUT = 0
         ERRTXT = 'FLUSHING VIS FILE'
         CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Update no. comp. left.
         XNCOMP = XNCOMP - MXCMP
C                                       Check if done.
         IF (XNCOMP.LE.0.01) GO TO 510
C                                       Input now output.
C                                       Close old input
         CALL ZCLOSE (LUNI, FINDI, IRET)
C                                       Set new input file name.
         IF (DISKO.LE.0) THEN
            VOL = SCRVOL(CNOSCO)
            CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
         ELSE
            VOL = DISKO
            CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
            END IF
C                                       Open new input file.
         ERRTXT = 'OPEN-FOR-READ VIS FILE'
         CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Open vis file for write.
C                                       Set output file name.
C                                       End big loop, N Comps per pass
 500     CONTINUE
C                                       Close files
 510  CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  WRITE(MSGTXT,2000,ERR=999) IRET, ERRTXT
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VSCDFT: Max. bandwidth smearing correction=',F7.0,'%')
 1001 FORMAT ('VSCDFT: Bandwidth correction factor =',F10.5)
 1002 FORMAT ('VSCDFT: Ref. freq. correction factor =',F10.5)
 1090 FORMAT ('VSCDFT: ERROR',I5,' READING CLEAN COMPS REC',I5)
 1240 FORMAT ('Model computation is ',I5,' percent complete')
 2000 FORMAT ('VSCDFT: ERROR',I5,' ',A)
      END
