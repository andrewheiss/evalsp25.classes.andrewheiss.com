

<!-- README.md is generated from README.qmd. Please edit that file -->

# Program Evaluation for Public Service <a href='https://evalsp25.classes.andrewheiss.com/'><img src='files/icon-512.png' align="right" height="139" /></a>

[PMAP 8521 • Spring 2025](https://evalsp25.classes.andrewheiss.com/)  
[Andrew Heiss](https://www.andrewheiss.com/) • Andrew Young School of
Policy Studies • Georgia State University

------------------------------------------------------------------------

**[Quarto](https://quarto.org/) +
[{targets}](https://docs.ropensci.org/targets/) +
[{renv}](https://rstudio.github.io/renv/) +
[{xaringan}](https://github.com/yihui/xaringan) = magic! 🧙**

------------------------------------------------------------------------

## How to build the site

1.  Install
    [RStudio](https://www.rstudio.com/products/rstudio/download/#download)
    version 2022.07.1 or later since it has a
    [Quarto](https://quarto.org/) installation embedded in it.
    Otherwise, download and install [Quarto](https://quarto.org/)
    separately.
2.  Open `evalsp25.Rproj` to open an [RStudio
    Project](https://r4ds.hadley.nz/workflow-scripts.html).
3.  If it’s not installed already, R *should* try to install the [{renv}
    package](https://rstudio.github.io/renv/) when you open the RStudio
    Project for the first time. If you don’t see a message about package
    installation, install it yourself by running
    `install.packages("renv")` in the R console.
4.  Run `renv::restore()` in the R console to install all the required
    packages for this project.
5.  Run `targets::tar_make()` in the R console to build everything.
6.  🎉 All done! 🎉 The complete website will be in a folder named
    `_site/`.

## {targets} pipeline

I use the [{targets} package](https://docs.ropensci.org/targets/) to
build this site and all its supporting files. The complete pipeline is
defined in [`_targets.R`](_targets.R) and can be run in the R console
with:

``` r
targets::tar_make()
```

The pipeline does several major tasks:

- **Create supporting data files**: The problem sets and examples I use
  throughout the course use many different datasets that come
  prepackaged in R packages, I downloaded from sources online, or that I
  generated myself. To make sure I and my students are using the latest,
  most correct datasets, the functions in [`R/tar_data.R`](R/tar_data.R)
  save and/or generate these datasets prior to building the website.

- **Compress project folders**: To make it easier to distribute problem
  sets and in-class activities to students, I compress all the folders
  in the [`/projects/`](./projects/) folder so that students can
  download and unzip a self-contained RStudio Project as a `.zip` file.
  These targets are [dynamically
  generated](https://books.ropensci.org/targets/dynamic.html) so that
  any new folder that is added to `/projects/` will automatically be
  zipped up when running the pipeline.

- **Render xaringan slides to HTML and PDF**: Quarto supports HTML-based
  slideshows through
  [reveal.js](https://quarto.org/docs/presentations/revealjs/). However,
  I created all my slides using
  [{xaringan}](https://github.com/yihui/xaringan), which is based on
  [remark.js](https://remarkjs.com/) and doesn’t work with Quarto.
  Since (1) I recorded all the class videos using my {xaringan} slides
  with a fancy template I made, and (2) I don’t want to recreate my
  fancy template in reveal.js yet, I want to keep using {xaringan}.

  The pipeline [dynamically generates
  targets](https://books.ropensci.org/targets/dynamic.html) for all the
  `.Rmd` files in [`/slides/`](./slides/) and renders them using R
  Markdown rather than Quarto.

  The pipeline then uses
  [{renderthis}](https://jhelvy.github.io/renderthis/) to convert each
  set of HTML slides into PDFs.

- **Build Quarto website**: This project is a [Quarto
  website](https://quarto.org/docs/websites/), which compiles and
  stitches together all the `.qmd` files in this project based on the
  settings in [`_quarto.yml`](_quarto.yml). See the [Quarto website
  documentation](https://quarto.org/docs/websites/) for more details.

- **Upload resulting `_site/` folder to my remote server**: Quarto
  places the compiled website in a folder named `/_site/`. The pipeline
  uses `rsync` to upload this folder to my personal remote server. This
  target will only run if the `UPLOAD_WEBSITES` environment variable is
  set to `TRUE`, and it will only work if you have an SSH key set up on
  my personal server, which only I do.

The complete pipeline looks like this:

<small>(This uses [`mermaid.js`
syntax](https://mermaid-js.github.io/mermaid/) and should display as a
graph on GitHub. You can also view it by pasting the code into
<https://mermaid.live>.)</small>

``` mermaid
graph LR
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Graph
    direction LR
    x13c17a05f1292bd5(["data_wage"]):::skipped --> xa2cea3ce7d59a063(["copy_wage"]):::queued
    x56b35e682aae9edd(["slide_rmd_10_slides_files"]):::skipped --> xe4f696ed33dca3d5["slide_rmd_10_slides"]:::queued
    x8a785351e8a12955(["copy_attendance"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x02ec1c8836b68307(["copy_barrels_obs"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x5c822725ce36c91b(["copy_barrels_rct"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    xb7ae1bbb98752be3(["copy_eitc"]):::skipped --> x5d6dea1a226f7537(["copy_data"]):::queued
    x12828badb2a666b8(["copy_evaluation"]):::skipped --> x5d6dea1a226f7537(["copy_data"]):::queued
    xd335c85feda5b133(["copy_food_health_politics"]):::skipped --> x5d6dea1a226f7537(["copy_data"]):::queued
    x34bc466d5242dbd2(["copy_monthly_panel"]):::skipped --> x5d6dea1a226f7537(["copy_data"]):::queued
    x89daf3c1eb54877b(["copy_penguins"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x0f4ea5caf3a6d1b7(["copy_plot_barrel_dag_obs"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x4b6dea362d287ce8(["copy_plot_barrel_dag_rct"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x47bfa1623a63c59a(["copy_public_housing"]):::skipped --> x5d6dea1a226f7537(["copy_data"]):::queued
    xa2cea3ce7d59a063(["copy_wage"]):::queued --> x5d6dea1a226f7537(["copy_data"]):::queued
    x376092e2d297750a(["slide_html_07_slides"]):::queued --> x0b8627ae1be8750b(["slide_pdf_07_slides"]):::queued
    xa94c97b5de950d8f(["proj_measurement_files"]):::skipped --> x2f1c64f2809aec21["proj_measurement"]:::queued
    x3fd07da3af5efd61(["gen_barrels"]):::skipped --> xab29c32e4997339f(["data_barrels_rct"]):::queued
    x0c6721b47332fe83(["slide_html_12_slides"]):::queued --> x9f307c61c0684b51(["slide_pdf_12_slides"]):::queued
    xb215bd886fd526a4(["slide_rmd_09_class_files"]):::skipped --> x6d23d539e3c300ef["slide_rmd_09_class"]:::queued
    xc0fdda873709f9c2(["gen_barrel_dags"]):::skipped --> xb44b7470225d14ba(["data_plot_barrel_dag_obs"]):::queued
    x51ffa3585e42bfea(["data_plot_barrel_dag_rct"]):::queued --> x4b6dea362d287ce8(["copy_plot_barrel_dag_rct"]):::queued
    x660da1d01e230321(["workflow_graph"]):::dispatched --> xc11069275cfeb620(["readme"]):::queued
    x39a82da8b6fae6fa["slide_rmd_01_slides"]:::queued --> x32a35e8ee71c7064(["slide_html_01_slides"]):::queued
    xd38256ee6c716da4(["slide_rmd_08_slides_files"]):::queued --> x1995dd2d77d423df["slide_rmd_08_slides"]:::queued
    xc1418dbce57ce8dc["slide_rmd_12_slides"]:::queued --> x0c6721b47332fe83(["slide_html_12_slides"]):::queued
    x65149f43519bdf7a(["proj_background_theory_files"]):::queued --> x5a7869c80aef4eb6["proj_background_theory"]:::queued
    x310b1f358ca634ee(["proj_final_project_files"]):::queued --> xe06674a27606ee5f["proj_final_project"]:::queued
    xb44b7470225d14ba(["data_plot_barrel_dag_obs"]):::queued --> x0f4ea5caf3a6d1b7(["copy_plot_barrel_dag_obs"]):::queued
    x011ded4e9e04173f(["data_penguins"]):::queued --> x89daf3c1eb54877b(["copy_penguins"]):::queued
    x16487c775dd503c0(["gen_nets"]):::queued --> x5c13df6a3d9972de(["data_nets"]):::queued
    xeb4b732280f64ef9(["slide_rmd_07_slides_files"]):::queued --> x670d8b11ea22361f["slide_rmd_07_slides"]:::queued
    xb03f1698e2debc94(["slide_rmd_12_slides_files"]):::queued --> xc1418dbce57ce8dc["slide_rmd_12_slides"]:::queued
    x527abcc5677dae68(["build_data"]):::queued --> xab1c89b73caca27f(["zip_proj_background_theory"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> xab1c89b73caca27f(["zip_proj_background_theory"]):::queued
    x5a7869c80aef4eb6["proj_background_theory"]:::queued --> xab1c89b73caca27f(["zip_proj_background_theory"]):::queued
    x3dd54da269b2735f["slide_rmd_05_slides"]:::queued --> xb91d3491b4ad9fcb(["slide_html_05_slides"]):::queued
    x32a35e8ee71c7064(["slide_html_01_slides"]):::queued --> xb5dcc63ac027309f(["slide_pdf_01_slides"]):::queued
    x83c90c487d16eadc(["schedule_file"]):::queued --> xd1e486155305a9d8(["schedule_ical_data"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> xc4d7385a98734101(["zip_proj_problem_set_2"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> xc4d7385a98734101(["zip_proj_problem_set_2"]):::queued
    xfa03902728fa3d47["proj_problem_set_2"]:::queued --> xc4d7385a98734101(["zip_proj_problem_set_2"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x2f7426ba30ba2e7e(["zip_proj_problem_set_3"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x2f7426ba30ba2e7e(["zip_proj_problem_set_3"]):::queued
    x46cce097a8e74d9c["proj_problem_set_3"]:::queued --> x2f7426ba30ba2e7e(["zip_proj_problem_set_3"]):::queued
    xf3073e7883bebc05(["gen_data_tutoring"]):::queued --> x33b05d4ba3aa2e1f(["gen_data_tutoring_fuzzy"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x7b102da22e2d9f74(["zip_proj_problem_set_4"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x7b102da22e2d9f74(["zip_proj_problem_set_4"]):::queued
    xdbf6198a6df96d09["proj_problem_set_4"]:::queued --> x7b102da22e2d9f74(["zip_proj_problem_set_4"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x7a9a0ac7b79ff64c(["zip_proj_problem_set_5"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x7a9a0ac7b79ff64c(["zip_proj_problem_set_5"]):::queued
    x13c438aa7f51f212["proj_problem_set_5"]:::queued --> x7a9a0ac7b79ff64c(["zip_proj_problem_set_5"]):::queued
    xb3a6182b19468761(["data_barrels_obs"]):::queued --> x02ec1c8836b68307(["copy_barrels_obs"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x33bb78c96c0a8294(["zip_proj_problem_set_6"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x33bb78c96c0a8294(["zip_proj_problem_set_6"]):::queued
    x7d5a049575ce3856["proj_problem_set_6"]:::queued --> x33bb78c96c0a8294(["zip_proj_problem_set_6"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x195c5f6488426358(["zip_proj_problem_set_7"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x195c5f6488426358(["zip_proj_problem_set_7"]):::queued
    x8746b08aa1ad3020["proj_problem_set_7"]:::queued --> x195c5f6488426358(["zip_proj_problem_set_7"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x86f1603e08b48abb(["zip_proj_problem_set_8"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x86f1603e08b48abb(["zip_proj_problem_set_8"]):::queued
    x4e5b0d5659b7cf79["proj_problem_set_8"]:::queued --> x86f1603e08b48abb(["zip_proj_problem_set_8"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x95db492e045f2365(["zip_proj_problem_set_9"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x95db492e045f2365(["zip_proj_problem_set_9"]):::queued
    x7ceb6132fcf83087["proj_problem_set_9"]:::queued --> x95db492e045f2365(["zip_proj_problem_set_9"]):::queued
    x3fd07da3af5efd61(["gen_barrels"]):::skipped --> xb3a6182b19468761(["data_barrels_obs"]):::queued
    x218c648ba493eca7(["slide_html_04_slides"]):::queued --> x8da37ce9706f9a14(["slide_pdf_04_slides"]):::queued
    x5001cb60de5aa7c8(["slide_rmd_01_slides_files"]):::skipped --> x39a82da8b6fae6fa["slide_rmd_01_slides"]:::queued
    xd58c0b90fe2777a2(["gen_data_tutoring_sharp"]):::queued --> x88d52c9eadd58930(["data_tutoring_sharp"]):::queued
    x882e10d77c8cabc1(["proj_problem_set_2_files"]):::queued --> xfa03902728fa3d47["proj_problem_set_2"]:::queued
    xec4213240ce10196(["proj_problem_set_3_files"]):::queued --> x46cce097a8e74d9c["proj_problem_set_3"]:::queued
    x0906f524f44befbc(["data_attendance"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xb3a6182b19468761(["data_barrels_obs"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xab29c32e4997339f(["data_barrels_rct"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x31155239efe31e96(["data_bed_nets_real"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x198ef0999f618b75(["data_bed_nets_time_machine"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x3a7fedbc278d1cb9(["data_card"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x58c9ab6b2b148a3b(["data_father_educ"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xae82ef75be61abf5(["data_gapminder"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x56dc2da91df821b4(["data_injury"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xed5ceb9aca28295f(["data_mpg"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x5c13df6a3d9972de(["data_nets"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x011ded4e9e04173f(["data_penguins"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xb44b7470225d14ba(["data_plot_barrel_dag_obs"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x51ffa3585e42bfea(["data_plot_barrel_dag_rct"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x2b011c827898b3f5(["data_tutoring_fuzzy"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x88d52c9eadd58930(["data_tutoring_sharp"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    xe7654a739ca89b04(["data_village_obs"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x4a46e55e02224ff6(["data_village_rct"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x13c17a05f1292bd5(["data_wage"]):::skipped --> x527abcc5677dae68(["build_data"]):::queued
    x140a279c1d99b677(["data_wage2"]):::queued --> x527abcc5677dae68(["build_data"]):::queued
    x788e4a75b279dfea(["proj_problem_set_4_files"]):::queued --> xdbf6198a6df96d09["proj_problem_set_4"]:::queued
    xffb836177b69392b(["proj_problem_set_5_files"]):::queued --> x13c438aa7f51f212["proj_problem_set_5"]:::queued
    x1ea35134a10eccd9(["gen_village"]):::queued --> xe7654a739ca89b04(["data_village_obs"]):::queued
    xba0eee0ae05b917e(["slide_html_03_slides"]):::queued --> x3f0ae6da980c2e5b(["slide_pdf_03_slides"]):::queued
    x17500053598366a3(["proj_problem_set_6_files"]):::queued --> x7d5a049575ce3856["proj_problem_set_6"]:::queued
    x96579a42e95a698c(["proj_problem_set_7_files"]):::queued --> x8746b08aa1ad3020["proj_problem_set_7"]:::queued
    x8a1bb306783c0878(["proj_problem_set_8_files"]):::queued --> x4e5b0d5659b7cf79["proj_problem_set_8"]:::queued
    xa024ffc60c26895b(["proj_problem_set_9_files"]):::queued --> x7ceb6132fcf83087["proj_problem_set_9"]:::queued
    xcc55386321c9e7e1(["slide_html_11_slides"]):::queued --> x16a4cfdac0e86ef7(["slide_pdf_11_slides"]):::queued
    x782368ccd11fc50b(["slide_rmd_04_slides_files"]):::queued --> xa5456a9999ae2296["slide_rmd_04_slides"]:::queued
    xc0fdda873709f9c2(["gen_barrel_dags"]):::skipped --> x51ffa3585e42bfea(["data_plot_barrel_dag_rct"]):::queued
    x5fee94802c729361(["site"]):::queued --> xd6774b1369562ec8(["deploy_site"]):::queued
    xab1c89b73caca27f(["zip_proj_background_theory"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x843abec8f5ade0b8(["zip_proj_causal_model"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x0a16a5802de381a3(["zip_proj_final_project"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    xb1a6df71aa4e43bc(["zip_proj_measurement"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    xc4d7385a98734101(["zip_proj_problem_set_2"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x2f7426ba30ba2e7e(["zip_proj_problem_set_3"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x7b102da22e2d9f74(["zip_proj_problem_set_4"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x7a9a0ac7b79ff64c(["zip_proj_problem_set_5"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x33bb78c96c0a8294(["zip_proj_problem_set_6"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x195c5f6488426358(["zip_proj_problem_set_7"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x86f1603e08b48abb(["zip_proj_problem_set_8"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x95db492e045f2365(["zip_proj_problem_set_9"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    x95f3d4f240416320(["zip_proj_threats_validity"]):::queued --> x949bf61054cdbfad(["all_zipped_projects"]):::queued
    xa5456a9999ae2296["slide_rmd_04_slides"]:::queued --> x218c648ba493eca7(["slide_html_04_slides"]):::queued
    x3e428f5776c79d29(["gen_data_bed_nets"]):::queued --> xb462657b7647cb6a(["gen_data_bed_nets_real"]):::queued
    x368f698220791499(["slide_html_06_slides"]):::queued --> x37dd1ea5a70b6ffc(["slide_pdf_06_slides"]):::queued
    x83c90c487d16eadc(["schedule_file"]):::queued --> x7f26ad8951796691(["schedule_page_data"]):::queued
    x2fc1e0904d94beca["slide_rmd_11_slides"]:::queued --> xcc55386321c9e7e1(["slide_html_11_slides"]):::queued
    xf97356b842ce00f0(["slide_html_14_slides"]):::queued --> x3c87d689cb981c63(["slide_pdf_14_slides"]):::queued
    x3ece8da1469c00db(["slide_rmd_03_slides_files"]):::queued --> x86cd11787895f0ce["slide_rmd_03_slides"]:::queued
    x527abcc5677dae68(["build_data"]):::queued --> x95f3d4f240416320(["zip_proj_threats_validity"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x95f3d4f240416320(["zip_proj_threats_validity"]):::queued
    x277edc977174b48e["proj_threats_validity"]:::queued --> x95f3d4f240416320(["zip_proj_threats_validity"]):::queued
    x86cd11787895f0ce["slide_rmd_03_slides"]:::queued --> xba0eee0ae05b917e(["slide_html_03_slides"]):::queued
    xd1e486155305a9d8(["schedule_ical_data"]):::queued --> xe96618267648362b(["schedule_ical_file"]):::queued
    x3633e9bc8b92fa35(["slide_rmd_11_slides_files"]):::queued --> x2fc1e0904d94beca["slide_rmd_11_slides"]:::queued
    x0dc1a8b47e76e805(["all_slides"]):::queued --> x5fee94802c729361(["site"]):::queued
    x949bf61054cdbfad(["all_zipped_projects"]):::queued --> x5fee94802c729361(["site"]):::queued
    xe96618267648362b(["schedule_ical_file"]):::queued --> x5fee94802c729361(["site"]):::queued
    x7f26ad8951796691(["schedule_page_data"]):::queued --> x5fee94802c729361(["site"]):::queued
    x7c3ed3bd4a1ecde3(["slide_html_13_slides"]):::queued --> xa515c2432f42a64b(["slide_pdf_13_slides"]):::queued
    xce2cc9c53c42aad7(["slide_rmd_06_slides_files"]):::queued --> x4e2decec9e1779c4["slide_rmd_06_slides"]:::queued
    xb462657b7647cb6a(["gen_data_bed_nets_real"]):::queued --> x31155239efe31e96(["data_bed_nets_real"]):::queued
    x4e2decec9e1779c4["slide_rmd_06_slides"]:::queued --> x368f698220791499(["slide_html_06_slides"]):::queued
    x0e9ba811e1b85e1c(["gen_attendance"]):::queued --> x0906f524f44befbc(["data_attendance"]):::queued
    x07f824c04f2508ef(["proj_threats_validity_files"]):::queued --> x277edc977174b48e["proj_threats_validity"]:::queued
    xcc033a60c97c8178(["gen_data_father_educ"]):::queued --> x58c9ab6b2b148a3b(["data_father_educ"]):::queued
    xb9d12f12d1b272aa(["slide_rmd_14_slides_files"]):::queued --> xa82e4c2714985d36["slide_rmd_14_slides"]:::queued
    x3a2f45bc039d5e77(["slide_html_09_class"]):::queued --> xf218b1e97c4fd1e7(["slide_pdf_09_class"]):::queued
    x3e428f5776c79d29(["gen_data_bed_nets"]):::queued --> x198ef0999f618b75(["data_bed_nets_time_machine"]):::queued
    x4d45c10c788cbb1e["slide_rmd_02_slides"]:::queued --> x3c2fdb322a18671d(["slide_html_02_slides"]):::queued
    x11762edb9ae29fab(["slide_rmd_13_slides_files"]):::skipped --> x5dee707e323d9bdb["slide_rmd_13_slides"]:::queued
    x33b05d4ba3aa2e1f(["gen_data_tutoring_fuzzy"]):::queued --> x2b011c827898b3f5(["data_tutoring_fuzzy"]):::queued
    xf3073e7883bebc05(["gen_data_tutoring"]):::queued --> xd58c0b90fe2777a2(["gen_data_tutoring_sharp"]):::queued
    x1995dd2d77d423df["slide_rmd_08_slides"]:::queued --> x8ccf1af7f721a931(["slide_html_08_slides"]):::queued
    x3c2fdb322a18671d(["slide_html_02_slides"]):::queued --> xd854d4a9c89216ab(["slide_pdf_02_slides"]):::queued
    x6d23d539e3c300ef["slide_rmd_09_class"]:::queued --> x3a2f45bc039d5e77(["slide_html_09_class"]):::queued
    xb91d3491b4ad9fcb(["slide_html_05_slides"]):::queued --> xd5b644ec3aff4c6c(["slide_pdf_05_slides"]):::queued
    x7a2aa5493a1e174f(["slide_html_10_slides"]):::queued --> x3a60c74acfe26c44(["slide_pdf_10_slides"]):::queued
    xa82e4c2714985d36["slide_rmd_14_slides"]:::queued --> xf97356b842ce00f0(["slide_html_14_slides"]):::queued
    x670d8b11ea22361f["slide_rmd_07_slides"]:::queued --> x376092e2d297750a(["slide_html_07_slides"]):::queued
    xb5dcc63ac027309f(["slide_pdf_01_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    xd854d4a9c89216ab(["slide_pdf_02_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x3f0ae6da980c2e5b(["slide_pdf_03_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x8da37ce9706f9a14(["slide_pdf_04_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    xd5b644ec3aff4c6c(["slide_pdf_05_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x37dd1ea5a70b6ffc(["slide_pdf_06_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x0b8627ae1be8750b(["slide_pdf_07_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    xb53859a3a1196b64(["slide_pdf_08_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    xf218b1e97c4fd1e7(["slide_pdf_09_class"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x3a60c74acfe26c44(["slide_pdf_10_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x16a4cfdac0e86ef7(["slide_pdf_11_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x9f307c61c0684b51(["slide_pdf_12_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    xa515c2432f42a64b(["slide_pdf_13_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x3c87d689cb981c63(["slide_pdf_14_slides"]):::queued --> x0dc1a8b47e76e805(["all_slides"]):::queued
    x353710180ed43191(["proj_causal_model_files"]):::queued --> x2c8618be2fe5cce5["proj_causal_model"]:::queued
    xab29c32e4997339f(["data_barrels_rct"]):::queued --> x5c822725ce36c91b(["copy_barrels_rct"]):::queued
    x0906f524f44befbc(["data_attendance"]):::queued --> x8a785351e8a12955(["copy_attendance"]):::queued
    xe4f696ed33dca3d5["slide_rmd_10_slides"]:::queued --> x7a2aa5493a1e174f(["slide_html_10_slides"]):::queued
    x8ccf1af7f721a931(["slide_html_08_slides"]):::queued --> xb53859a3a1196b64(["slide_pdf_08_slides"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x843abec8f5ade0b8(["zip_proj_causal_model"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x843abec8f5ade0b8(["zip_proj_causal_model"]):::queued
    x2c8618be2fe5cce5["proj_causal_model"]:::queued --> x843abec8f5ade0b8(["zip_proj_causal_model"]):::queued
    x5dee707e323d9bdb["slide_rmd_13_slides"]:::queued --> x7c3ed3bd4a1ecde3(["slide_html_13_slides"]):::queued
    xdf57381c885cd00f(["slide_rmd_02_slides_files"]):::queued --> x4d45c10c788cbb1e["slide_rmd_02_slides"]:::queued
    x527abcc5677dae68(["build_data"]):::queued --> xb1a6df71aa4e43bc(["zip_proj_measurement"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> xb1a6df71aa4e43bc(["zip_proj_measurement"]):::queued
    x2f1c64f2809aec21["proj_measurement"]:::queued --> xb1a6df71aa4e43bc(["zip_proj_measurement"]):::queued
    x1ea35134a10eccd9(["gen_village"]):::queued --> x4a46e55e02224ff6(["data_village_rct"]):::queued
    x527abcc5677dae68(["build_data"]):::queued --> x0a16a5802de381a3(["zip_proj_final_project"]):::queued
    x5d6dea1a226f7537(["copy_data"]):::queued --> x0a16a5802de381a3(["zip_proj_final_project"]):::queued
    xe06674a27606ee5f["proj_final_project"]:::queued --> x0a16a5802de381a3(["zip_proj_final_project"]):::queued
    x78de70e29b2b5fc4(["slide_rmd_05_slides_files"]):::queued --> x3dd54da269b2735f["slide_rmd_05_slides"]:::queued
    x830adcacfab4076a(["deploy_script"]):::skipped --> x830adcacfab4076a(["deploy_script"]):::skipped
  end
```

## Fonts and colors

The fonts used throughout the site are [Fira Sans
Condensed](https://fonts.google.com/specimen/Fira+Sans+Condensed) (for
headings and titles) and
[Barlow](https://fonts.google.com/specimen/Barlow) (for everything
else).

The colors for the site and hex logo come from a palette of 8 colors
generated from the [viridis inferno color
map](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html#the-color-scales):

``` r
viridisLite::viridis(8, option = "inferno", begin = 0.1, end = 0.9)
```

<img src="README_files/figure-commonmark/show-inferno-1.png"
width="768" />

## Licenses

**Text and figures:** All prose and images are licensed under Creative
Commons ([CC-BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/))

**Code:** All code is licensed under the [MIT License](LICENSE.md).
