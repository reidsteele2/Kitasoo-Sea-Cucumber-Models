data{
    int<lower=1> N;                   // Number of data points
    int Osyt[N];                      // Counts
    row_vector[5] xi;                 // Vector of initial x values for each site. Should all be 0.999
    int year[N];                      // year, first year set to 0
    matrix[10, 5] HY;                 // matrix of harvest in each site (column) for each year, duplicated to put into months (row)
    int site[N];                      // site identifier. Modified from 0,2,4,8,16 to 1-5 to make equivalent to matrix column numbers
    int length[N];                    // Length of the transects
    int transect[N];                  // Transect ID number, set to start at 1
    int resample[N];                  // Resample flag, 1 = resampled data, 0 = not resampled data
    matrix[10, 5] mweights;           // Mean biosample weights for each site and year
    matrix[10, 5] sdweights;          // SD biosample weights for each site and year
    row_vector[5] V;                  // Virgin individuals estimate for each site
}

parameters{
    real<lower=0.001,upper=0.999> xmax;           // X value at which maximum latent productivity occurs, 0-1 bounded
    real<lower=0.5,upper=0.999> b;                          // Shaping parameter, 0-1 bounded
    real<lower=0.001,upper=0.999> LPmax;            // Maximum latent productivity
    real<lower=0.002,upper=5> G;                  // Geometric mean of biomass density (kg/m^2)
    real<lower=0.011,upper=0.754> Wsyt[10,5];     // Sea cucumber average weight, bounded by max and min
    real<lower=0> sigS;              // Standard deviation of random site effects
    real<lower=0> sigY;              // Standard deviation of random year effects
    real<lower=0> sigT;              // Standard deviation of random transect effects
    real hat_Tst[87];                                 // Random transect effects
    real hat_Yy[10];                                  // Random year effects
    real hat_Ss[5];                                   // Random site effects
    real T;                                       // Tst hierarchy
    real Y;                                       // Yy hierarchy
    real S;                                       // Ss hierarchy
}

transformed parameters{
    real Tst[87];                                 // Random transect effects
    real Yy[10];                                  // Random year effects
    real Ss[5];                                   // Random site effects
    matrix[11, 5] xt;                                            // Matrix of x(t) values
    matrix[10, 5] LP;                                            // Matrix of LP values
    matrix[10, 5] dLP;                                           // Matrix of dLP values
    matrix[10, 5] Et;                                            // Matrix of harvest rate
    real<lower=0.001,upper=0.999> a;
    a = (b * (xmax / (1 - xmax ) ));                              // Calculation of a from Hajas(2011)
    for ( i in 1:87) {
        Tst[i] = T + (hat_Tst[i] * sigT );                              // Tst priors for each transect, noninformative, from Hajas(2011)
    }
    for ( i in 1:10) {
        Yy[i] = Y + (hat_Yy[i] * sigY );                               // Yy priors for each year, noninformative, from Hajas(2011)
    }
    for ( i in 1:5) {
        Ss[i] = S + (hat_Ss[i] * sigS );                               // Ss priors for each site, noninformative, from Hajas(2011)
    }
    for ( j in 1:5 ) {
        xt[1, j] = xi[j];                                             // First row of matrix xt is xi
        for ( i in 2:11 ){                                            // For loop designed to run simulations to fill Et, xt, LP, and dLP matrices
            Et[i-1, j] = (HY[i-1, j] * 0.453592) / (V[j] * Wsyt[1, j]);                   // Exploitation rate calculated from catch divided by virgin number, both converted to mass. Quota for site 0 is a random guess, but value should be mostly irrelevant. Can be improved if desired.
            if ( xt[i-1, j] >= 1 ) {                                  // If statement telling the program to reset x to 0.999 if it goes equal to or above 1 (goes undefined at 1, constrained to less than 1)
                xt[i-1, j] = 0.999;
                }
          if ( xt[i-1, j] <= 0 ) {                                    // If statement telling the program to set xt, LP, and dLP to 0 for the rest of the simulation if they ever reach or exceed 0
              xt[i, j] = 0;
              xt[i-1, j] = 0;
              LP[i-1, j] = 0;
              dLP[i-1, j] = 0;
              }
          else {
              LP[i-1, j] = (LPmax * ((xt[i-1, j]/xmax)^a) * ((1 - xt[i-1, j])/(1 - xmax))^b);                                // Calculation of LP
              dLP[i-1, j] = (LP[i-1, j] * (LP[i-1, j] - Et[i-1, j]) * ((a/xt[i-1, j]) - (b/(1 - xt[i-1, j]))));                // Calculation of dx2/dt
              xt[i, j] = xt[i-1, j] + ((1.0) * (LP[i-1, j] - Et[i-1, j])) + ((((1.0)^2.0)/2.0) * dLP[i-1, j]);                         // Calculation of x(t+1)
              if (xt[i-1,j] >= 0.999){
                  xt[i-1,j] = 1;                                                                                           // Change 0.999s in xt back into 1s for accuracy in generating xy, fixed Laredo Hajas problems (to an extent)
                }
            }
        }
    }
}

model{
    vector[N] xy;                                                 // x(y) value plugged into survey equation
    vector[N] Nsyt;                                               // Number of expected sea cucumbers
    sigT ~ normal( 1, 1);                                 // sigT prior, noninformative, from Hajas (2011)
    sigY ~ normal( 1, 1);                                 // sigY prior, noninformative, from Hajas (2011)
    sigS ~ normal( 1, 1);                                 // sigS prior, noninformative, from Hajas (2011)
    T ~ normal( 0 , 0.5 );                                          // T hyperprior
    Y ~ normal( 0 , 0.5 );                                          // Y hyperprior
    S ~ normal( 0 , 0.5 );                                          // S hyperprior
    for ( i in 1:10 ) {
      for ( j in 1:5 ) {
        Wsyt[i, j] ~ normal( mweights[i,j] , sdweights[i,j] );    // Wsyt prior, informative, mean and SD of biosamples in this EFA
      }
    }
    G ~ exponential(1);                                           // G prior. Comes partially from looking at data and partially from interpretation of Hajas (2011). Seems to outperform a standard uniform prior.
//    LPmax ~ uniform( 0.01 , 0.25 );                               // LPmax prior, noninformative, from Hajas (2011)
    b ~ uniform(0.001,0.999);                                           // b prior, noninformative, from Hajas (2011)
    xmax ~ uniform(0.001, 0.999);                                 // xmax prior, noninformative, from Hajas (2011)
    hat_Tst ~ normal( 0 , 1 );
    hat_Yy ~ normal( 0 , 1 );
    hat_Ss ~ normal( 0 , 1 );
    for ( i in 1:N ) {                                            // For loop designed to calculate survey component from Hajas(2011)
        xy[i] = xt[year[i]+1, site[i]];                                                                                                             // Extraction of xy for each data point
        if ( resample[N] == 0 ) {
            Nsyt[i] = (((G * exp(Ss[site[i]] + Yy[year[i]+1] + Tst[transect[i]])) * xy[i]) * (length[i]*4))  /  Wsyt[year[i]+1, site[i]];           // Survey component calculation - non-resampled data
        } else {
            Nsyt[i] = (((G * exp(Ss[site[i]] + Yy[year[i]+1] + Tst[transect[i]])) * (xy[i] - Et[year[i]+1, site[i]])) * (length[i]*4))  /  Wsyt[year[i]+1, site[i]];           // Survey component calculation - resampled data
        }
    }
    Osyt ~ poisson( Nsyt );                                                                                           // Test of expected vs observed values
}

generated quantities{                                                                                                 // Generated quantities is the exact same as the model block except it lacks the last line, and calculated deviance (dev)
    vector[N] xy;
    vector[N] Nsyt;
    vector[N] log_lik;
    real dev;
    dev = 0;
    for ( i in 1:N ) {                                            // For loop designed to calculate survey component from Hajas(2011)
        xy[i] = xt[year[i]+1, site[i]];                                                                                                             // Extraction of xy for each data point
        if ( resample[N] == 0 ) {
            Nsyt[i] = (((G * exp(Ss[site[i]] + Yy[year[i]+1] + Tst[transect[i]])) * xy[i]) * (length[i]*4))  /  Wsyt[year[i]+1, site[i]];           // Survey component calculation - non-resampled data
        } else {
            Nsyt[i] = (((G * exp(Ss[site[i]] + Yy[year[i]+1] + Tst[transect[i]])) * (xy[i] - Et[year[i]+1, site[i]])) * (length[i]*4))  /  Wsyt[year[i]+1, site[i]];           // Survey component calculation - resampled data
        }
    }
    dev = dev + (-2)*poisson_lpmf( Osyt | Nsyt );                                                                     // Deviance information criterion calculation
    for (i in 1:N){                                                                                                   // log likelihood calculation
      log_lik[i] = poisson_lpmf( Osyt[i] | Nsyt[i] );
    }
}
