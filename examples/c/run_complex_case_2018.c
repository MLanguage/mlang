#include "ir_complex_case_2018.h"

int main()
{
    m_input input;
    m_empty_input(&input);
    input.v_0AM = m_literal(1);     // Case a cocher : situation de famille Maries
    input.v_0CF = m_literal(1);     // Nombre d'enfants mineurs ou handicapes
    input.v_1AX = m_literal(10000); // CIMR - salaires revenus exceptionnels - dec1
    input.v_4XD = m_literal(4000);  // CIMR - recettes exceptionnelles non retenues
    input.v_1AO = m_literal(5000);  // Pensions alimentaires percues - Declarant 1
    input.v_1BS = m_literal(25000); // Pensions, retraites, rentes - Declarant 2
    input.v_4BE = m_literal(4000);  // Regime Micro-foncier - Recettes brutes     // Salaires - Declarant 1

    m_output output;
    for (int i = 39000; i <= 40000; i++)
    {
        input.v_1AJ = m_literal(i);
        m_extracted(&output, &input);
    };
    printf("IAN: %.2f\n", output.v_IAN.value);
    printf("IINET: %.2f\n", output.v_IINET.value);
    printf("IRNET: %.2f\n", output.v_IRNET.value);
    printf("NAPCR: %.2f\n", output.v_NAPCR.value);
    printf("TXMOYIMP: %.2f\n", output.v_TXMOYIMP.value);
    printf("NBPT: %.2f\n", output.v_NBPT.value);
    printf("CSG: %.2f\n", output.v_CSG.value);
    printf("RDSN: %.2f\n", output.v_RDSN.value);
    printf("PSOL: %.2f\n", output.v_PSOL.value);
    printf("CIRM: %.2f\n", output.v_CIMR.value);
    printf("REVKIRE: %.2f\n", output.v_REVKIRE.value);
    return 0;
}
