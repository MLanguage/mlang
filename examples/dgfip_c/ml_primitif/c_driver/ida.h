#ifndef __IDA_H__
#define __IDA_H__

#include <liste.h>
#include <commun.h>

extern void infoActVide(void);
extern void infoActAide(void);
extern void infoActFmt(void);
extern void infoActTrt(void);
extern void infoRep(char *nom);
extern void infoReg(char *nom);
extern void infoOk(char *fich);
extern void infoKo(char *fich);
extern void infoKc(char *fich);
extern void infoNbOk(int ok, int tot);
extern void infoNbKo(int ko, int tot);
extern void infoNbKc(int kc, int tot);
extern void infoNonRec(char *dir);
extern void infoLien(char *nom);

extern int discoOptsRecDup(int b);
extern int discoOptsStrictDup(int b);
extern int discoOptsAnneeParDefaut(int annee, int b);
extern int discoOptsModeDup(int b);
extern int discoOptsAnneeDup(int b);
extern int discoRep(char *nom, L_char l);
extern int discoStat(char *nom, int b);
extern int discoFichier(char *nom, int b);
extern int discoAnneeRevenu(int anneeCalc, int anneeRevenu);

extern void anoOptsExe(void);
extern void anoOptsTrop(char *arg);
extern void anoOptsAction(char *act);
extern void anoOptsCat(char *cat);
extern void anoOptsInc(char *arg);
extern void anoOptsModeAbs(void);
extern void anoOptsModeArg(char *arg);
extern void anoOptsModeAbsent(void);
extern void anoOptsModeDup(T_mode mode0, T_mode mode1);
extern void anoOptsAnneeAbs(void);
extern void anoOptsAnneeArg(char *arg);
extern void anoOptsAnneeDup(int annee0, int annee1);
extern void anoOptsDefValArg(char *arg);
extern void anoOptsDefVarArg(char *arg);
extern void anoLigneInvalide(int ligne, int err);
extern void anoCodeVide(int ligne);
extern void anoVarAbs(char *var);
extern void anoErrEnTrop(char *err);
extern void anoErrNonRecue(char *err);
extern void anoValeurFausse(char *nom, double val, double valRes);

#endif /* __IDA_H__ */
