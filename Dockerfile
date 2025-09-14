FROM condaforge/mambaforge:24.3.0-0

# Set non-interactive frontend and configure timezone for Asia/Dubai
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Dubai

# Install and configure timezone non-interactively
RUN apt-get update && apt-get install -y --no-install-recommends tzdata && \
    ln -fs /usr/share/zoneinfo/Asia/Dubai /etc/localtime && \
    dpkg-reconfigure --frontend noninteractive tzdata && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install system dependencies including nginx and supervisor
RUN apt-get update && apt-get install -y \
    nginx \
    supervisor \
    && rm -rf /var/lib/apt/lists/*

# Add a non-privileged user for nginx
RUN adduser --system --no-create-home --shell /bin/false --group --disabled-login nginx

# Copy environment.yaml first to leverage Docker cache
COPY environment.yaml /srv/shiny-server/environment.yaml

# Create the conda environment from the YAML file
RUN mamba env create -f /srv/shiny-server/environment.yaml

# Set the PATH to include the conda environment
ENV PATH /opt/conda/envs/merged_env/bin:$PATH

# Combine all R package installations into a single layer
RUN conda run -n merged_env R -e "\
    install.packages(c('smplot2', 'GOplot', 'wordcloud2', 'Seurat'), repos='http://cran.rstudio.com/'); \
    remotes::install_github('bnprks/BPCells/r'); \
    devtools::install_github('mahmoudibrahim/genesorteR'); \
    remotes::install_github('satijalab/seurat-wrappers'); \
    remotes::install_github('daqana/dqshiny')"


#Installation of monocle
#RUN conda run -n merged_env R -e "remotes::install_github('bnprks/BPCells/r')"
#RUN conda run -n merged_env R -e "devtools::install_github('cole-trapnell-lab/monocle3')"

SHELL ["/bin/bash", "-c"]

RUN source activate merged_env && \
    R -e "devtools::install_github('cole-trapnell-lab/monocle3', force = TRUE)"




# Clean stale data
RUN mamba clean -a -y

COPY monocle3 /srv/shiny-server/monocle3

# # Copy BSgenome package
# COPY BSgenome.Hsapiens.UCSC.hg19_1.4.3.tar.gz /

# Copy configuration files
COPY nginx.conf /etc/nginx/nginx.conf
COPY uiApp/src/out/ /usr/share/nginx/html
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Update supervisor configuration with correct path
RUN sed -i 's|/opt/conda/envs/v_[^/]*|/opt/conda/envs/merged_env|g' /etc/supervisor/conf.d/supervisord.conf

# Expose the port for the Nginx server
EXPOSE 80

# Command to start supervisord
CMD ["/usr/bin/supervisord"]
