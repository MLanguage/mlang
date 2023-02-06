
#include <stdlib.h>
#include <stdio.h>

#include "ir_all_ins_selected_outs_2018.h"

#include "irdata.h"
#include "desc.h"

int main()
{
    T_irdata *irdata = IRDATA_new_irdata();
    IRDATA_reset_irdata(irdata);
    IRDATA_range(irdata, desc_0AM, 1.0);     // Case a cocher : situation de famille Maries
    IRDATA_range(irdata, desc_0CF, 1.0);     // Nombre d'enfants mineurs ou handicapes
    IRDATA_range(irdata, desc_1AX, 10000.0); // CIMR - salaires revenus exceptionnels - dec1
    IRDATA_range(irdata, desc_4XD, 4000.0);  // CIMR - recettes exceptionnelles non retenues
    IRDATA_range(irdata, desc_1AO, 5000.0);  // Pensions alimentaires percues - Declarant 1
    IRDATA_range(irdata, desc_1BS, 25000.0); // Pensions, retraites, rentes - Declarant 2
    IRDATA_range(irdata, desc_4BE, 4000.0);  // Regime Micro-foncier - Recettes brutes

    for (int i = 39000; i <= 40000; i++)
    {
        IRDATA_range(irdata, desc_1AJ, (double)i); // Salaires - Declarant 1
        dgfip_calculation(irdata);
    };

    printf("IAN: %.2f\n", *IRDATA_extrait_special(irdata, desc_IAN));
    printf("IINET: %.2f\n", *IRDATA_extrait_special(irdata, desc_IINET));
    printf("IRNET: %.2f\n", *IRDATA_extrait_special(irdata, desc_IRNET));
    printf("NAPCR: %.2f\n", *IRDATA_extrait_special(irdata, desc_NAPCR));
    printf("TXMOYIMP: %.2f\n", *IRDATA_extrait_special(irdata, desc_TXMOYIMP));
    printf("NBPT: %.2f\n", *IRDATA_extrait_special(irdata, desc_NBPT));
    printf("CSG: %.2f\n", *IRDATA_extrait_special(irdata, desc_CSG));
    printf("RDSN: %.2f\n", *IRDATA_extrait_special(irdata, desc_RDSN));
    printf("PSOL: %.2f\n", *IRDATA_extrait_special(irdata, desc_PSOL));
    printf("CIRM: %.2f\n", *IRDATA_extrait_special(irdata, desc_CIMR));
    printf("REVKIRE: %.2f\n", *IRDATA_extrait_special(irdata, desc_REVKIRE));

    IRDATA_delete_irdata(irdata);

    return 0;
}
