#---- Loading packages ----
install.packages('tidyverse')
library('tidyverse')
tidyverse_packages()


#---- Reading in data and data exploration ----

dat <- read.csv("data/raw_morph.csv") # reading in data

# Stolon internode

pdf("figures/exploration/stolon.pdf")

ggplot(data = dat, aes(x = Species, y = `SIL`)) + geom_boxplot() + labs(title = "Box Plot of stolon internode length by Species",x = "Species", y = "Stolon internode length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `SIB`)) + geom_boxplot() + labs(title = "Box Plot of stolon internode breadth by species",x = "Species", y = "Stolon breadth length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Peduncle

pdf("figures/exploration/penducle.pdf")

ggplot(data = dat, aes(x = Species, y = `PL`)) + geom_boxplot() + labs(title = "Box Plot of penduncle length by Species",x = "Species", y = "Penduncle length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `PB`)) + geom_boxplot() + labs(title = "Box Plot of peduncle breadth by species",x = "Species", y = "Peduncle breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Leaf sheath

pdf("figures/exploration/leaf sheath.pdf")

ggplot(data = dat, aes(x = Species, y = `LSL`)) + geom_boxplot() + labs(title = "Box Plot of leaf sheath length by Species",x = "Species", y = "Leaf sheath length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `LSB`)) + geom_boxplot() + labs(title = "Box Plot of leaf sheath breadth by species",x = "Species", y = "leaf sheath breadth  (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Leaf blade

pdf("figures/exploration/leaf blade.pdf")

ggplot(data = dat, aes(x = Species, y = `LBL`)) + geom_boxplot() + labs(title = "Box Plot of leaf blade length by Species",x = "Species", y = "Leaf blade length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `LBB`)) + geom_boxplot() + labs(title = "Box Plot of leaf blade breadth by species",x = "Species", y = "Leaf blade breadth  (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Inflorescence 

pdf("figures/exploration/inflorescence.pdf")

ggplot(data = dat, aes(x = Species, y = `IL`)) + geom_boxplot() + labs(title = "Box Plot of inflorescence length by Species",x = "Species", y = "Inflorescence length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `IB`)) + geom_boxplot() + labs(title = "Box Plot of inflorescence breadth by species",x = "Species", y = "Inflorescence breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Inflorescence bract

pdf("figures/exploration/inflorescence bract.pdf")

ggplot(data = dat, aes(x = Species, y = `IBL`)) + geom_boxplot() + labs(title = "Box Plot of inflorescence bract length by Species",x = "Species", y = "Inflorescence bract length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `IBB`)) + geom_boxplot() + labs(title = "Box Plot of inflorescence bract breadth by species",x = "Species", y = "Inflorescence bract breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Spikelet

pdf("figures/exploration/spikelet.pdf")

ggplot(data = dat, aes(x = Species, y = `SN`)) + geom_boxplot() + labs(title = "Box Plot of spikelet number by species",x = "Species", y = "Spikelet number") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
                                                                            
ggplot(data = dat, aes(x = Species, y = `SL`)) + geom_boxplot() + labs(title = "Box Plot of spikelet length by Species",x = "Species", y = "Spikelet length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `SB`)) + geom_boxplot() + labs(title = "Box Plot of spikelet breadth by species",x = "Species", y = "Spikelet breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Glume 

pdf("figures/exploration/glume.pdf")

ggplot(data = dat, aes(x = Species, y = `GN`)) + geom_boxplot() + labs(title = "Box Plot of glume number by Species",x = "Species", y = "Glume number") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `ML`)) + geom_boxplot() + labs(title = "Box Plot of mucro length by species",x = "Species", y = "Mucro length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `GL`)) + geom_boxplot() + labs(title = "Box Plot of glume length by Species",x = "Species", y = "Glume length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `GB`)) + geom_boxplot() + labs(title = "Box Plot of glume breadth by species",x = "Species", y = "Glume breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Anther length 

pdf("figures/exploration/anther.pdf")

ggplot(data = dat, aes(x = Species, y = `AL`)) + geom_boxplot() + labs(title = "Box Plot of anther length by Species",x = "Species", y = "Anther length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()

# Nutlet 

pdf("figures/exploration/nutlet.pdf")

ggplot(data = dat, aes(x = Species, y = `NL`)) + geom_boxplot() + labs(title = "Box Plot of inflorescence length by Species",x = "Species", y = "Nutlet length (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(data = dat, aes(x = Species, y = `NB`)) + geom_boxplot() + labs(title = "Box Plot of nutlet breadth by species",x = "Species", y = "Nutlet breadth (mm)") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.off()