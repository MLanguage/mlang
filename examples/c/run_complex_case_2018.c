#include "ir_complex_case_2018.c"

int main()
{

    m_output output;
    for (int i = 11000; i <= 40000; i++)
    {
        output = m_extracted((struct m_input){
            .v_0AM = m_literal(1),     // Case a cocher : situation de famille Maries
            .v_0CF = m_literal(1),     // Nombre d'enfants mineurs ou handicapes
            .v_1AX = m_literal(10000), // CIMR - salaires revenus exceptionnels - dec1
            .v_4XD = m_literal(4000),  // CIMR - recettes exceptionnelles non retenues
            .v_1AO = m_literal(5000),  // Pensions alimentaires percues - Declarant 1
            .v_1BS = m_literal(25000), // Pensions, retraites, rentes - Declarant 2
            .v_4BE = m_literal(4000),  // Regime Micro-foncier - Recettes brutes
            .v_1AJ = m_literal(i),     // Salaires - Declarant 1
        });
        if (i % 1000 == 0)
        {
            printf("With 1AJ = %d, TXMOYIMP: %.2f\n", i, output.txmoyimp_222120_1.value);
        };
    };
    printf("IAN: %.2f\n", output.ian_301050_1.value);
    printf("IINET: %.2f\n", output.iinet_221190_0.value);
    printf("IRNET: %.2f\n", output.irnet_221230_0.value);
    printf("NAPCR: %.2f\n", output.napcr_221940_0.value);
    printf("TXMOYIMP: %.2f\n", output.txmoyimp_222120_1.value);
    printf("NBPT: %.2f\n", output.nbpt_601000_0.value);
    printf("CSG: %.2f\n", output.csg_101210_0.value);
    printf("RDSN: %.2f\n", output.rdsn_101210_1.value);
    printf("PSOL: %.2f\n", output.psol_101210_2.value);
    printf("CIRM: %.2f\n", output.cimr_201908_0.value);
    printf("REVKIRE: %.2f\n", output.revkire_871125_0.value);
    return 0;
}