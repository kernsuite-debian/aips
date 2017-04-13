      SUBROUTINE FACSET (IVOL, CNO, INIF, INSRC, MODEL, INFAC, IRET)
C-----------------------------------------------------------------------
C! Sets FACGRD for subtraction/division by source models
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2005-2006, 2008-2009, 2012, 2015
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
C   FACSET adjusts the factor to be applied to the data model when
C   subtracting form or dividing into the data themselves.  The
C   parameter, FACGRD, in DGDS,INC is set 1 over the Clean beam area in
C   pixels for IMAG models (and a nasty warning is issued).  If the UV
C   data set is multi-source and the SU table has a flux value for the
C   source, then FACGRD = SU_flux / Sum(all CCs in model).
C   SETGDS must have been called before this routine.
C   Inputs:
C      IVOL     I      UV file disk number
C      CNO      I      UV file catalog number
C      INIF     I      Current IF to use
C      INSRC    I      Current source number: 0 -> single source file
C      MODEL    I      1 -> CC model, 2,3 -> image model
C      INFAC    R      Users rquested factor
C   In/out:
C   Output:
C   Common variables used:
C      NONEG    L      Negatives not allowed
C      LIMFLX   R      No components < LIMFLX allowed (in abs value)
C      NCLNG    I(*)   Max component number by facet
C      NSUBG    I(*)   First component number by facet
C      CCDISK   I(*)   CC file disk number by facet
C      CCCNO    I(*)   CC file catalog number by facet
C      CCVER    I(*)   CC file version number by facet
C      MFIELD   I      Number of facets
C-----------------------------------------------------------------------
      INTEGER   IVOL, CNO, INIF, INSRC, MODEL, IRET
      REAL      INFAC
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYS(7)*8, UTYPE*8, VELDEF*8, VELTYP*8, CALCOD*4,
     *   SOUNAM*16, KNOSOU(4,4)*16
      REAL      RADIUS, AREA, CATR(256), FLUX(4,MAXIF), TFLUX, SFLUX,
     *   RECORD(20), KNORAD(4), R
      HOLLERITH CATH(256)
      INTEGER   CATCLN(256), ITYPE, LSRC, IDSOU, SUKOLS(MAXSUC), TLUN,
     *   SUNUMV(MAXSUC), QUAL, NUMIF, J, NSOURC, I, ISURNO, SUFQID,
     *   NFIELD, MSGSAV, BUFFER(512), JERR, LFIELD, KOLS(7), RAKOL,
     *   DECKOL, FLXKOL, TYPKOL, MAJKOL, MINKOL, PAKOL, NCC, NKEY,
     *   JLREC, JNREC
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF),
     *   BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, PMRA, PMDEC, RAOBS,
     *   DECOBS
      LOGICAL  TABLE, EXIST, FITASC
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATCLN, CATH, CATR)
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5) ,MAJKOL),               (KOLS(6), MINKOL),
     *   (KOLS(7), PAKOL)
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ',
     *   'TYPE OBJ', 'MAJOR AX', 'MINOR AX', 'POSANGLE'/
      DATA TLUN /30/
      DATA KNORAD /3.0, 0.95, 0.85, 0.75/
      DATA KNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C138',   '0518+165', '0521+166', 'J0521+1638'/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      IF (INFAC.NE.0.0) THEN
         FACGRD(1) = INFAC
      ELSE
         FACGRD(1) = 1.0
         END IF
C                                       Image model
      IF ((MODEL.EQ.2) .OR. (MODEL.EQ.3)) THEN
C                                       Get image header
         CALL CATIO ('READ', CCDISK(1), CCCNO(1), CATCLN, 'REST',
     *      SCRBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING IMAGE HEADER'
            GO TO 990
            END IF
C                                       Is it a Clean image?
         CALL H2CHR (8, 1, CATH(KHBUN), UTYPE)
         CALL CHLTOU (8, UTYPE)
         AREA = ABS (CATR(KRCIC) * CATR(KRCIC+1))
         IF (AREA.GT.0.0) AREA = 1.1331 * CATR(KRBMJ)*CATR(KRBMN) / AREA
         ITYPE = CATCLN(KITYP)
C                                       maybe
         IF ((AREA.GT.0.0) .OR. (ITYPE.EQ.1) .OR. (UTYPE.EQ.'JY/BEAM'))
     *      THEN
            IF ((AREA.GT.0.0) .AND. (ITYPE.EQ.1) .AND.
     *         (UTYPE.EQ.'JY/BEAM')) THEN
               MSGTXT = 'YOU ARE USING A CLEAN IMAGE FOR A MODEL'
               CALL MSGWRT (7)
               MSGTXT = 'THIS SHOULD NOT MATCH THE DATA AT ALL WELL'
               IF (AREA.NE.1.0) CALL MSGWRT (7)
               FACGRD(1) = FACGRD(1) / AREA
            ELSE IF ((ITYPE.EQ.0) .AND. (AREA.GT.0.) .AND.
     *         (UTYPE.EQ.'JY/BEAM')) THEN
               MSGTXT = 'YOU ARE USING A DIRTY IMAGE FOR A MODEL'
               CALL MSGWRT (7)
               FACGRD(1) = FACGRD(1) / AREA
               END IF
            END IF
C                                       CC model
      ELSE IF (MODEL.EQ.1) THEN
         LSRC = INSRC
C                                       See if SU file exists.
         IF (LSRC.GT.0) THEN
            CALL ISTAB ('SU', IVOL, CNO, 1, TLUN, BUFFER, TABLE, EXIST,
     *         FITASC, JERR)
            IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) THEN
               LSRC = 0
               MSGTXT = 'FACSET: EXPECTED SOURCE TABLE MISSING'
               IF (INSRC.GT.1) CALL MSGWRT (6)
               END IF
            END IF
C                                       find UV header
         TFLUX = 0.0
         RADIUS = 180.0
         NFIELD = MFIELD
         IF (LSRC.GT.0) THEN
            CALL CATIO ('READ', IVOL, CNO, CATCLN, 'REST', SCRBLK, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING UV HEADER'
               GO TO 990
               END IF
C                                       find source flux
            CALL SOUINI ('READ', BUFFER, IVOL, CNO, 1, CATCLN, TLUN,
     *         NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *         JERR)
            NSOURC = BUFFER(5)
            DO 110 I = 1,NSOURC
C                                       Read record
               CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IRET)
C                                       Check error
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING SOURCE TABLE'
                  GO TO 990
                  END IF
C                                       source matches
               IF (LSRC.EQ.IDSOU) THEN
                  J = MAX (1, MIN (INIF, NUMIF))
                  TFLUX = FLUX (1,J)
                  GO TO 120
                  END IF
 110           CONTINUE
            WRITE (MSGTXT,1110) LSRC
            CALL MSGWRT (7)
 120        CALL TABIO ('CLOS', 0, I, BUFFER, BUFFER, JERR)
            IF (TFLUX.GT.0.0) THEN
               WRITE (MSGTXT,1120) TFLUX
               CALL MSGWRT (3)
               DO 130 I = 1,4
                  DO 125 J = 1,4
                     IF (SOUNAM.EQ.KNOSOU(J,I)) THEN
                        RADIUS = KNORAD(I) / 3600.0
                        NFIELD = 1
                        GO TO 140
                        END IF
 125                 CONTINUE
 130              CONTINUE
            ELSE
               MSGTXT = 'NO FLUX FOUND FOR CALIBRATOR SOURCE IN' //
     *            ' SU TABLE'
               CALL MSGWRT (8)
               MSGTXT = 'MODEL WILL USE THE FLUX CONTAINED IN IT ONLY'
               CALL MSGWRT (8)
               MSGTXT = 'THE USE OF GETJY AFTER THIS WILL BE INCORRECT'
               IF (TSKNAM.EQ.'CALIB') CALL MSGWRT (8)
               END IF
            END IF
C                                       Find CC flux
 140     SFLUX = 0.0
         NCC = 0
         RADIUS = RADIUS * RADIUS
         DO 190 LFIELD = 1,NFIELD
            IF (NCLNG(LFIELD).GE.NSUBG(LFIELD)) THEN
               CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *            CATCLN, 'REST', SCRBLK, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING CC IMAGE HEADER'
                  GO TO 990
                  END IF
               JNREC = 1
               JLREC = 0
               NKEY = 0
               CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *            ABS(CCVER(LFIELD)), CATCLN, TLUN, NKEY, JNREC, JLREC,
     *            SCRBLK, BUFFER, IRET)
               IF (IRET.GT.1) THEN
                  WRITE (MSGTXT,1000) IRET, 'OPENING CC FILE'
                  GO TO 990
                  END IF
C                                       Find columns (physical)
               NKEY = JLREC
               TYPKOL = 7
               IF (JLREC.GT.7) NKEY = 7
               CALL FNDCOL (NKEY, KEYS, 8, .FALSE., BUFFER, KOLS, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'FINDING CC COLUMNS'
                  GO TO 990
                  END IF
               DO 170 J = NSUBG(LFIELD),NCLNG(LFIELD)
                  CALL TABIO ('READ', 0, J, RECORD, BUFFER, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING CC FILE'
                     GO TO 990
                  ELSE IF (IRET.EQ.0) THEN
                     IF ((NONEG .AND. (RECORD(FLXKOL).LE.0.0)) .OR.
     *                  (ABS(RECORD(FLXKOL)).LT.LIMFLX)) GO TO 180
                     R = RECORD(RAKOL)*RECORD(RAKOL) +
     *                  RECORD(DECKOL)*RECORD(DECKOL)
                     IF (R.LE.RADIUS) SFLUX = SFLUX + RECORD(FLXKOL)
                     NCC = NCC + 1
                     END IF
 170              CONTINUE
 180           CALL TABIO ('CLOS', 0, J, RECORD, BUFFER, IRET)
               END IF
 190        CONTINUE
         WRITE (MSGTXT,1190) SFLUX, NCC
         CALL REFRMT (MSGTXT, ' ', J)
         CALL MSGWRT (3)
         IF (SFLUX.GT.0.0) SFLUX = TFLUX / SFLUX
         IF (SFLUX.GT.0.0) FACGRD(1) = FACGRD(1) * SFLUX
         IF (NFIELD.LT.MFIELD) THEN
            MSGTXT = 'FACSET: used only facet 1 for standard' //
     *         ' calibrator source'
            CALL MSGWRT (3)
            END IF
C                                       MODEL wrong
      ELSE
         WRITE (MSGTXT,1195) MODEL
         IRET = 1
         GO TO 990
         END IF
C
      WRITE (MSGTXT,1200) ABS (FACGRD(1))
      IF (ABS(FACGRD(1)).NE.1.0) CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FACSET: ERROR',I3,1X,A)
 1110 FORMAT ('FACSET: SOURCE NUMBER',I5,' NOT FOUND!!!')
 1120 FORMAT ('FACSET: source model will be scaled to',F7.3,' Jy')
 1190 FORMAT ('FACSET:',F12.6,' Jy found from',I12,' components')
 1195 FORMAT ('FACSET: MODEL =',I3,' UNRECOGNIZED')
 1200 FORMAT ('FACSET: scaling factor set to',1PE13.5)
      END
