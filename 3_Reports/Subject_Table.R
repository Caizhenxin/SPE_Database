setwd("D:/xiazai/SPE_Database-main_test")

# 预设的标准列顺序
DEFAULT_STANDARD_COLUMNS <- c(
  "Subject_ID", "Exp_id", "Age", "Gender", "Handedness", "Ethnicity", 
  "Employment_Status", "Country", "First_Language", "Education"
)

# 数据框列提取和标准化函数 - 完整版
extract_and_standardize <- function(df, 
                                    selected_cols = NULL, 
                                    rename_map = NULL, 
                                    standard_order = NULL,
                                    fill_value = "/",
                                    exp_id_value = NULL,
                                    deduplicate_by_subject = TRUE,
                                    deduplicate_column = "Subject_ID",  # 新参数：自定义去重列名
                                    verbose = TRUE) {
  
  if (verbose) {
    cat("=== 开始数据框列提取和标准化 ===\n")
    cat("原始维度:", nrow(df), "行 ×", ncol(df), "列\n")
    cat("原始列名:", paste(colnames(df), collapse = ", "), "\n\n")
  }
  
  # 步骤1: 选择指定列（如果提供）
  if (!is.null(selected_cols)) {
    if (verbose) cat("步骤1: 选择指定列\n")
    
    # 检查哪些列存在
    existing_cols <- intersect(selected_cols, colnames(df))
    missing_cols <- setdiff(selected_cols, colnames(df))
    
    if (length(missing_cols) > 0 && verbose) {
      cat("  ⚠️  未找到列:", paste(missing_cols, collapse = ", "), "\n")
    }
    
    if (length(existing_cols) > 0) {
      df <- df[, existing_cols, drop = FALSE]
      if (verbose) {
        cat("  ✓ 已选择列:", paste(existing_cols, collapse = ", "), "\n")
      }
    } else {
      stop("错误: 没有找到任何指定的列！")
    }
  }
  
  # 步骤2: 重命名列（如果提供）
  if (!is.null(rename_map)) {
    if (verbose) cat("\n步骤2: 重命名列\n")
    
    for (old_name in names(rename_map)) {
      new_name <- rename_map[[old_name]]
      if (old_name %in% colnames(df)) {
        colnames(df)[colnames(df) == old_name] <- new_name
        if (verbose) cat("  ✓", old_name, "→", new_name, "\n")
      } else if (verbose) {
        cat("  ⚠️  未找到列:", old_name, "\n")
      }
    }
  }
  
  # 步骤3: 按标准顺序排列（如果提供）
  if (!is.null(standard_order)) {
    if (verbose) cat("\n步骤3: 按标准顺序排列并填充缺失列\n")
    
    # 检查并创建缺失的标准列（但不覆盖已存在的列）
    for (col_name in standard_order) {
      if (!col_name %in% colnames(df)) {
        df[[col_name]] <- fill_value
        if (verbose) cat("  + 创建新列:", col_name, "（内容:", fill_value, "）\n")
      } else if (verbose) {
        cat("  ✓ 列已存在:", col_name, "\n")
      }
    }
    
    # 特殊处理：如果提供了exp_id_value，直接赋值给Exp_id列
    if (!is.null(exp_id_value) && "Exp_id" %in% colnames(df)) {
      df$Exp_id <- exp_id_value
      if (verbose) cat("  ✓ 已设置 Exp_id =", exp_id_value, "\n")
    }
    
    # 获取额外列（不在标准顺序中的列）
    extra_cols <- setdiff(colnames(df), standard_order)
    if (length(extra_cols) > 0 && verbose) {
      cat("  发现额外列:", paste(extra_cols, collapse = ", "), "\n")
    }
    
    # 重新排列列顺序
    final_order <- c(standard_order, extra_cols)
    df <- df[, final_order, drop = FALSE]
    
    if (verbose) cat("  ✓ 列已按标准顺序排列\n")
  }
  
  # 步骤4: 按指定列去重（如果启用且存在该列）
  if (deduplicate_by_subject && deduplicate_column %in% colnames(df)) {
    if (verbose) {
      cat("\n步骤4: 按", deduplicate_column, "列去重\n")
      cat("  去重前行数:", nrow(df), "\n")
    }
    
    # 检查重复情况
    duplicates <- duplicated(df[[deduplicate_column]])
    n_duplicates <- sum(duplicates)
    
    if (n_duplicates > 0) {
      if (verbose) {
        cat("  发现", n_duplicates, "行重复的", deduplicate_column, "\n")
        
        # 显示重复的值
        dup_values <- unique(df[[deduplicate_column]][duplicates])
        if (length(dup_values) <= 10) {
          cat("  重复的", deduplicate_column, ":", paste(dup_values, collapse = ", "), "\n")
        } else {
          cat("  重复的", deduplicate_column, ":", paste(dup_values[1:10], collapse = ", "), "... (共", length(dup_values), "个)\n")
        }
      }
      
      # 去重：保留每个值的第一行（强制保持数据框格式）
      df <- df[!duplicates, , drop = FALSE]
      
      if (verbose) {
        cat("  ✓ 已去重，保留每个", deduplicate_column, "的第一行\n")
        cat("  去重后行数:", nrow(df), "\n")
      }
    } else {
      if (verbose) {
        cat("  ✓ 没有发现重复的", deduplicate_column, "\n")
      }
    }
  } else if (deduplicate_by_subject && !deduplicate_column %in% colnames(df)) {
    if (verbose) {
      cat("\n步骤4: 跳过去重（未找到", deduplicate_column, "列）\n")
    }
  } else if (verbose) {
    cat("\n步骤4: 跳过去重（已禁用）\n")
  }
  
  # 最终检查数据框完整性
  if (!is.data.frame(df)) {
    if (verbose) cat("  ⚠️  警告：结果不是数据框，正在转换...\n")
    df <- as.data.frame(df)
  }
  
  # 生成报告
  if (verbose) {
    cat("\n=== 处理完成报告 ===\n")
    cat("最终维度:", nrow(df), "行 ×", ncol(df), "列\n")
    cat("最终列名:\n")
    for (i in 1:length(colnames(df))) {
      col_name <- colnames(df)[i]
      if (!is.null(standard_order) && col_name %in% standard_order) {
        cat(sprintf("  %2d. %s\n", i, col_name))
      } else {
        cat(sprintf("  %2d. %s (额外列)\n", i, col_name))
      }
    }
    cat("数据类型:", class(df), "\n")
  }
  
  return(df)
}

# 便捷函数：快速设置标准列顺序
set_standard_columns <- function(...) {
  return(c(...))
}

# 便捷函数：快速设置重命名映射
set_rename_mapping <- function(...) {
  return(list(...))
}

# 简化版函数：使用预设标准列
standardize_dataframe <- function(df, 
                                  selected_cols = NULL, 
                                  rename_map = NULL, 
                                  additional_standard_cols = NULL,
                                  fill_value = "/",
                                  exp_id_value = NULL,
                                  deduplicate_by_subject = TRUE,
                                  deduplicate_column = "Subject_ID",  # 新参数：自定义去重列名
                                  verbose = TRUE) {
  
  # 合并预设标准列和额外标准列
  standard_cols <- DEFAULT_STANDARD_COLUMNS
  if (!is.null(additional_standard_cols)) {
    standard_cols <- c(standard_cols, additional_standard_cols)
  }
  
  return(extract_and_standardize(
    df = df,
    selected_cols = selected_cols,
    rename_map = rename_map,
    standard_order = standard_cols,
    fill_value = fill_value,
    exp_id_value = exp_id_value,
    deduplicate_by_subject = deduplicate_by_subject,
    deduplicate_column = deduplicate_column,  # 传递新参数
    verbose = verbose
  ))
}

# 使用示例

result <- extract_and_standardize(
  df = data, # 放入原始数据
  selected_cols = c("Pair.Number", "Handedness"), # 提取想要后续操作的原始数据中的列
  rename_map = list("Pair.Number" = "Pair_ID",    # 对替换所需原始数据中的列名
                    "Participant.2.Age" = "P2_Age",
                    "Participant.1.Age" = "P1_Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,      # 生成数据中的列的顺序
  exp_id_value = "P6E1",                          # 设置实验编号
  deduplicate_by_subject = TRUE,                  # 是否去重（默认按照Subject_ID去重）
  deduplicate_column = "Pair_ID",  # 增加此列可以自定义去重列名
  verbose = TRUE
)


# Amodeo_2024_CABN
# 读取CSV文件
data_1 <- read.csv("./2_Raw_Data/Amodeo_2024_CABN/Amodeo_2024_CABN_Exp1_raw.csv")

result_1 <- extract_and_standardize(
  df = data_1,
  selected_cols = c("Subject"),
  rename_map = list("Subject" = "Subject_ID"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt28E1",  # 🆕 新参数：直接设置实验ID
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_1$Autism <- ifelse(result_1$Subject_ID < 50, 0, 1)

# Constable_2019_EPHPP
# Exp1
data_2 <- read.csv("./2_Raw_Data/Constable_2019_EPHPP/Exp1/Constable_2019_EPHPP_Exp1_raw.csv")

result_2 <- extract_and_standardize(
  df = data_2,
  selected_cols = c("Participant.Number", "Handedness", "First.Language", "Gender", "Age"),
  rename_map = list("Participant.Number" = "Subject_ID",
                    "First.Language" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P5E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_2$Handedness <- ifelse(result_2$Handedness == 'r', 'R', 'L')
result_2$First_Language <- 'Hungarian'

# Exp2
data_3 <- read.csv("./2_Raw_Data/Constable_2019_EPHPP/Exp2/Constable_2019_EPHPP_Exp2_raw.csv")

result_3 <- extract_and_standardize(
  df = data_3,
  selected_cols = c("Participant.Number", "Handedness", "First.Language", "Gender", "Age"),
  rename_map = list("Participant.Number" = "Subject_ID",
                    "First.Language" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P5E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_3$Handedness <- ifelse(result_3$Handedness == 'r', 'R', 'L')
result_3$First_Language <- ifelse(result_3$First_Language == 'h', 'Hungarian', '/')

# Exp3
data_4 <- read.csv("./2_Raw_Data/Constable_2019_EPHPP/Exp3/Constable_2019_EPHPP_Exp3_raw.csv")

result_4 <- extract_and_standardize(
  df = data_4,
  selected_cols = c("Participant.Number", "Handedness", "First.Language", "Gender", "Age"),
  rename_map = list("Participant.Number" = "Subject_ID",
                    "First.Language" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P5E3", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_4$Handedness <- ifelse(result_4$Handedness == 'r', 'R', 'L')
result_4$First_Language <- 'Hungarian'

# Exp4
data_5 <- read.csv("./2_Raw_Data/Constable_2019_EPHPP/Exp4/Constable_2019_EPHPP_Exp4_raw.csv")

DEFAULT_STANDARD_COLUMNS_P5E4 <- c(
  "Pair_ID", "Exp_id", "P1_Age", "P2_Age", "Gender", "Handedness", "Ethnicity", 
  "Employment_Status", "Country", "First_Language", "Education"
)

result_5 <- extract_and_standardize(
  df = data_5,
  selected_cols = c("Pair.Number", "Handedness", "First.Language", "P2Age","P1Age"),
  rename_map = list("Pair.Number" = "Pair_ID",
                    "P2Age" = "P2_Age",
                    "P1Age" = "P1_Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS_P5E4,
  exp_id_value = "P5E4", 
  deduplicate_by_subject = TRUE,
  deduplicate_column = "Pair_ID", # 按Pair_ID去重
  verbose = TRUE
)

result_5$First_Language <- 'Hungarian'

# Constable_2020_AP

data_6 <- read.csv("./2_Raw_Data/Constable_2020_AP/Constable_2020_AP_Exp1_raw.csv")

DEFAULT_STANDARD_COLUMNS_P6E1 <- c(
  "Pair_ID", "Exp_id", "P1_Age", "P2_Age", "Gender", "Handedness", "Ethnicity", 
  "Employment_Status", "Country", "First_Language", "Education"
)

result_6 <- extract_and_standardize(
  df = data_6,
  selected_cols = c("Pair.Number", "Handedness", "First.Language", "Participant.1.Age","Participant.2.Age"),
  rename_map = list("Pair.Number" = "Pair_ID",
                    "Participant.2.Age" = "P2_Age",
                    "Participant.1.Age" = "P1_Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS_P6E1,
  exp_id_value = "P6E1", 
  deduplicate_by_subject = TRUE,
  deduplicate_column = "Pair_ID", # 按Pair_ID去重
  verbose = TRUE
)

# Constable_2020_CE
# Exp1

data_7 <- read.csv("./2_Raw_Data/Constable_2020_CE/Exp1/Constable_2020_CE_Exp1_raw.csv")

result_7 <- extract_and_standardize(
  df = data_7,
  selected_cols = c("X1..Participant.Number", "X4..Handedness", "X5..First.Lanugage", "X3..Age", "X2..Gender..M.F."),
  rename_map = list("X1..Participant.Number" = "Subject_ID",
                    "X4..Handedness" = "Handedness",
                    "X5..First.Lanugage" = "First_Language",
                    "X3..Age" = "Age",
                    "X2..Gender..M.F." = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P46E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_7$Handedness[substr(tolower(result_7$Handedness), 1, 1) == "r"] <- "R"
result_7$Handedness[substr(tolower(result_7$Handedness), 1, 1) == "l"] <- "L"

# Exp2

data_8 <- read.csv("./2_Raw_Data/Constable_2020_CE/Exp2/Constable_2020_CE_Exp2_raw.csv")

print(head(data_8, 5))

result_8 <- extract_and_standardize(
  df = data_8,
  selected_cols = c("Subject", "X4..Handedness", "X5..First.Language", "X3..Age", "X2..Gender"),
  rename_map = list("Subject" = "Subject_ID",
                    "X4..Handedness" = "Handedness",
                    "X5..First.Language" = "First_Language",
                    "X3..Age" = "Age",
                    "X2..Gender" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P46E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Dalmaso_2024_CC
# Exp1 

if (!exists("data_9") || nrow(data_9) == 0) {
  cat("\n方法3：尝试使用readr包\n")
  if (!require(readr, quietly = TRUE)) {
    cat("需要安装readr包：install.packages('readr')\n")
  } else {
    tryCatch({
      data_9 <- read_csv("./2_Raw_Data/Dalmaso_2024_CC/Experiment_1_Japan/2_raw_self_JP.csv", 
                         locale = locale(encoding = "UTF-8"))
      cat("✓ readr包UTF-8编码读取成功！\n")
      print(paste("数据维度:", nrow(data_9), "行 ×", ncol(data_9), "列"))
    }, error = function(e) {
      cat("✗ readr包UTF-8编码读取失败\n")
      
      # 尝试自动检测编码
      tryCatch({
        data_9 <- read_csv("./2_Raw_Data/Dalmaso_2024_CC/Experiment_1_Japan/2_raw_self_JP.csv")
        cat("✓ readr包自动检测编码成功！\n")
        print(paste("数据维度:", nrow(data_9), "行 ×", ncol(data_9), "列"))
      }, error = function(e) {
        cat("✗ readr包自动检测编码失败\n")
      })
    })
  }
}

print(head(data_9, 5))

result_9 <- extract_and_standardize(
  df = data_9,
  selected_cols = c("participant", "sex", "age (years)", "dominant hand"),
  rename_map = list("participant" = "Subject_ID",
                    "dominant hand" = "Handedness",
                    "age (years)" = "Age",
                    "sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pu2E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_9$Handedness[substr(tolower(result_9$Handedness), 1, 1) == "r"] <- "R"
result_9$Handedness[substr(tolower(result_9$Handedness), 1, 1) == "l"] <- "L"

result_9$Gender[result_9$Gender == "male"] <- "Male"
result_9$Gender[result_9$Gender == "female"] <- "Female"

# Exp2

if (!exists("data_10") || nrow(data_10) == 0) {
  cat("\n方法3：尝试使用readr包\n")
  if (!require(readr, quietly = TRUE)) {
    cat("需要安装readr包：install.packages('readr')\n")
  } else {
    tryCatch({
      data_10 <- read_csv("./2_Raw_Data/Dalmaso_2024_CC/Experiment_2_Italy/1_Info_Participants_IT.csv", 
                         locale = locale(encoding = "UTF-8"))
      cat("✓ readr包UTF-8编码读取成功！\n")
      print(paste("数据维度:", nrow(data_10), "行 ×", ncol(data_10), "列"))
    }, error = function(e) {
      cat("✗ readr包UTF-8编码读取失败\n")
      
      # 尝试自动检测编码
      tryCatch({
        data_10 <- read_csv("./2_Raw_Data/Dalmaso_2024_CC/Experiment_2_Italy/1_Info_Participants_IT.csv")
        cat("✓ readr包自动检测编码成功！\n")
        print(paste("数据维度:", nrow(data_10), "行 ×", ncol(data_10), "列"))
      }, error = function(e) {
        cat("✗ readr包自动检测编码失败\n")
      })
    })
  }
}

print(head(data_10, 5))

result_10 <- extract_and_standardize(
  df = data_10,
  selected_cols = c("participant", "sex", "age (years)"),
  rename_map = list("participant" = "Subject_ID",
                    "age (years)" = "Age",
                    "sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pu2E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_10$Gender[result_10$Gender == "male"] <- "Male"
result_10$Gender[result_10$Gender == "female"] <- "Female"

# Feldborg_2021_ERPH
# Exp1

data_11 <- read.csv("./2_Raw_Data/Feldborg_2021_ERPH/Feldborg_2021_ERPH_Exp1_raw.csv")

print(head(data_11, 5))

result_11 <- extract_and_standardize(
  df = data_11,
  selected_cols = c("subject", "SubGender", "group", "SubAge", "SubEthnicity", "SubHand"),
  rename_map = list("subject" = "Subject_ID",
                    "SubAge" = "Age",
                    "SubGender" = "Gender",
                    "SubEthnicity" = "Ethnicity",
                    "SubHand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt13E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_11$Handedness[result_11$Handedness == "Right"] <- "R"
result_11$Handedness[result_11$Handedness == "Left"] <- "L"
result_11$Handedness[result_11$Handedness == "Ambidextrous"] <- "A"

# Haciahmet_2023_Psy

data_12 <- read.csv("./2_Raw_Data/Haciahmet_2023_Psy/Haciahmet_2023_Psy_Exp1_raw.csv")

print(head(data_12, 5))

result_12 <- extract_and_standardize(
  df = data_12,
  selected_cols = c("Subject", "Alter", "Hand", "Geschlecht"),
  rename_map = list("Subject" = "Subject_ID",
                    "Alter" = "Age",
                    "Geschlecht" = "Gender",
                    "Hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt1E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_12$Gender[substr(result_12$Gender, 1, 4) == "Mann"] <- "Male"
result_12$Gender[substr(result_12$Gender, 1, 4) == "Frau"] <- "Female"

result_12$Handedness[substr(tolower(result_12$Handedness), 1, 1) == "r"] <- "R"
result_12$Handedness[substr(tolower(result_12$Handedness), 1, 1) == "l"] <- "L"

# Hu_2020_CP
# Exp1

data_13 <- read.csv("./2_Raw_Data/Hu_2020_CP/Hu_2020_CP_Exp1_raw.csv")

result_13 <- extract_and_standardize(
  df = data_13,
  selected_cols = c("Subject", "Sex", "Hand", "Age"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "Hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P20E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_13$Gender[substr(tolower(result_13$Gender), 1, 6) == "female"] <- "Female"
result_13$Gender[substr(tolower(result_13$Gender), 1, 4) == "male"] <- "Male"

# Hu_2023

data_14 <- read.csv("./2_Raw_Data/Hu_2023/Hu_2023_Exp1_raw.csv")

result_14 <- extract_and_standardize(
  df = data_14,
  selected_cols = c("Subject", "Sex", "Handedness", "Age"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps2E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_14$Gender[substr(tolower(result_14$Gender), 1, 6) == "female"] <- "Female"
result_14$Gender[substr(tolower(result_14$Gender), 1, 4) == "male"] <- "Male"

result_14$Handedness[substr(tolower(result_14$Handedness), 1, 1) == "r"] <- "R"
result_14$Handedness[substr(tolower(result_14$Handedness), 1, 1) == "l"] <- "L"

# Kolvoort_2020_HBM

data_15 <- read.csv("./2_Raw_Data/Kolvoort_2020_HBM/Kolvoort_2020_HBM_Exp1_foreff_raw.csv")

# 预设的标准列顺序
DEFAULT_STANDARD_COLUMNS <- c(
  "Subject_ID", "Exp_id", "Age", "Gender", "Handedness", "Ethnicity", 
  "Employment_Status", "Country", "First_Language", "Education"
)

result_15 <- extract_and_standardize(
  df = data_15,
  selected_cols = c("subid", "completededucation", "handedness", "age","firstlanguage", "countryself"),
  rename_map = list("subid" = "Subject_ID",
                    "completededucation" = "Education",
                    "handedness" = "Handedness",
                    "firstlanguage" = "First_Language",
                    "countryself" = "Country",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P34E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_15$Handedness[substr(tolower(result_15$Handedness), 1, 1) == "r"] <- "R"
result_15$Handedness[substr(tolower(result_15$Handedness), 1, 1) == "l"] <- "L"

# Liang_2022_HBM

data_16 <- read.csv("./2_Raw_Data/Liang_2022_HBM/Liang_2022_HBM_Exp1_raw.csv") 

result_16 <- extract_and_standardize(
  df = data_16,
  selected_cols = c("subj_idx", "age", "gender"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "gender" = "Gender",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt3E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_16$Gender[substr(tolower(result_16$Gender), 1, 6) == "female"] <- "Female"
result_16$Gender[substr(tolower(result_16$Gender), 1, 4) == "male"] <- "Male"

# Liu_2023_CP

data_17 <- read.csv("./2_Raw_Data/Liu_2023_CP/Liu_2023_CP_Exp1_raw.csv")  

result_17 <- extract_and_standardize(
  df = data_17,
  selected_cols = c("Participant.Public.ID", "age", "gender"),
  rename_map = list("Participant.Public.ID" = "Subject_ID",
                    "gender" = "Gender",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps1E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Martinez-Perez_2024_CC

data_18 <- read.csv("./2_Raw_Data/Martinez-Perez_2024_CC/Martinez-Perez_2024_CC_Exp2_raw.csv") 

result_18 <- extract_and_standardize(
  df = data_18,
  selected_cols = c("Subject", "Edad", "Mano", "Sexo"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sexo" = "Gender",
                    "Edad" = "Age",
                    "Mano" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt27E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_18$Gender[tolower(result_18$Gender) == "mujer"] <- "Female"
result_18$Gender[tolower(result_18$Gender) == "hombre"] <- "Male"

result_18$Handedness <- as.character(result_18$Handedness)
result_18$Handedness[tolower(result_18$Handedness) == "diestro"] <- "R"
result_18$Handedness[tolower(result_18$Handedness) == "zurdo"] <- "L"

# Navon_2021
# Exp1

data_19 <- read.csv("./2_Raw_Data/Navon_2021/Navon_2021_Exp1_raw.csv") 

result_19 <- extract_and_standardize(
  df = data_19,
  selected_cols = c("Subject", "Age", "Handedness", "MotherTongue", "Sex"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "MotherTongue" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pn13E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_19$Gender[tolower(result_19$Gender) == "female"] <- "Female"
result_19$Gender[tolower(result_19$Gender) == "male"] <- "Male"

result_19$Handedness[tolower(result_19$Handedness) == "right"] <- "R"
result_19$Handedness[tolower(result_19$Handedness) == "left"] <- "L"

# Exp2

data_20 <- read.csv("./2_Raw_Data/Navon_2021/Navon_2021_Exp2_raw.csv") 

result_20 <- extract_and_standardize(
  df = data_20,
  selected_cols = c("Subject", "Age", "Handedness", "MotherTongue", "Sex"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "MotherTongue" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pn13E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_20$Gender[tolower(result_20$Gender) == "female"] <- "Female"
result_20$Gender[tolower(result_20$Gender) == "male"] <- "Male"

result_20$Handedness[tolower(result_20$Handedness) == "right"] <- "R"
result_20$Handedness[tolower(result_20$Handedness) == "left"] <- "L"

# Exp3

data_21 <- read.csv("./2_Raw_Data/Navon_2021/Navon_2021_Exp3_raw.csv") 

result_21 <- extract_and_standardize(
  df = data_21,
  selected_cols = c("Subject", "Age", "Handedness", "MotherTongue", "Sex"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "MotherTongue" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pn13E3", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_21$Gender[tolower(result_21$Gender) == "female"] <- "Female"
result_21$Gender[tolower(result_21$Gender) == "male"] <- "Male"

result_21$Handedness[tolower(result_21$Handedness) == "right"] <- "R"
result_21$Handedness[tolower(result_21$Handedness) == "left"] <- "L"

# Exp4

data_22 <- read.csv("./2_Raw_Data/Navon_2021/Navon_2021_Exp3_raw.csv") 

result_22 <- extract_and_standardize(
  df = data_22,
  selected_cols = c("Subject", "Age", "Handedness", "MotherTongue", "Sex"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "MotherTongue" = "First_Language"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pn13E4", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_22$Gender[tolower(result_22$Gender) == "female"] <- "Female"
result_22$Gender[tolower(result_22$Gender) == "male"] <- "Male"

result_22$Handedness[tolower(result_22$Handedness) == "right"] <- "R"
result_22$Handedness[tolower(result_22$Handedness) == "left"] <- "L"

# Perrykkad_2022_BMC

data_23 <- read.csv("./2_Raw_Data/Perrykkad_2022_BMC/Perrykkad_2022_BMC_qs_raw.csv") 
 
result_23 <- extract_and_standardize(
  df = data_23,
  selected_cols = c("anonID", "Year.of.Birth", "AQ", "BPQ","SPQ","SCCS","SCIM","IBI","BAI","BDI","SCIM_DI","SCIM_CI","SCIM_LoI","Gender.Identity"),
  rename_map = list("anonID" = "Subject_ID",
                    "Gender.Identity" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt7E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Qian_2019_QJEP
# Exp1

data_24 <- read.csv("./2_Raw_Data/Qian_2019_QJEP/Qian_2019_QJEP_Exp1_raw.csv") 

result_24 <- extract_and_standardize(
  df = data_24,
  selected_cols = c("Subject", "Age", "Sex"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P51E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_24$Gender[tolower(result_24$Gender) == "female"] <- "Female"
result_24$Gender[tolower(result_24$Gender) == "male"] <- "Male"

result_24$Handedness <-  'R'
result_24$Country <-  'China'

# Exp2a

data_25 <- read.csv("./2_Raw_Data/Qian_2019_QJEP/Qian_2019_QJEP_Exp2_raw.csv") 

result_25 <- extract_and_standardize(
  df = data_25,
  selected_cols = c("Subject", "Age", "Sex", "Handedness"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P51E2a", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_25$Gender[tolower(result_25$Gender) == "female"] <- "Female"
result_25$Gender[tolower(result_25$Gender) == "male"] <- "Male"

result_25$Country <-  'China'
result_25$Handedness[tolower(result_25$Handedness) == "right"] <- "R"

# Exp2b

data_26 <- read.csv("./2_Raw_Data/Qian_2019_QJEP/Qian_2019_QJEP_Exp2b_raw.csv") 

result_26 <- extract_and_standardize(
  df = data_26,
  selected_cols = c("Subject", "Age", "Sex", "Handedness"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P51E2b", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_26$Gender[tolower(result_26$Gender) == "female"] <- "Female"
result_26$Gender[tolower(result_26$Gender) == "male"] <- "Male"

result_26$Country <-  'China'
result_26$Handedness[tolower(result_26$Handedness) == "right"] <- "R"

# Schaefer_2019_CP
# Exp1 

data_27 <- read.csv("./2_Raw_Data/Schaefer_2019_CP/Schaefer_2019_CP_Exp1_raw.csv") 

result_27 <- extract_and_standardize(
  df = data_27,
  selected_cols = c("Subject", "Age", "Sex", "hand"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P54E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_27$Gender[tolower(result_27$Gender) == "female"] <- "Female"
result_27$Gender[tolower(result_27$Gender) == "male"] <- "Male"

result_27$Handedness[tolower(result_27$Handedness) == "right"] <- "R"
result_27$Handedness[tolower(result_27$Handedness) == "left"] <- "L"

# Exp2

data_28 <- read.csv("./2_Raw_Data/Schaefer_2019_CP/Schaefer_2019_CP_Exp2_raw.csv") 

result_28 <- extract_and_standardize(
  df = data_28,
  selected_cols = c("Subject", "Age", "Sex", "hand"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P54E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_28$Gender[tolower(result_28$Gender) == "female"] <- "Female"
result_28$Gender[tolower(result_28$Gender) == "male"] <- "Male"

result_28$Handedness[tolower(result_28$Handedness) == "right"] <- "R"
result_28$Handedness[tolower(result_28$Handedness) == "left"] <- "L"

# Exp3

data_29 <- read.csv("./2_Raw_Data/Schaefer_2019_CP/Schaefer_2019_CP_Exp3_raw.csv") 

result_29 <- extract_and_standardize(
  df = data_29,
  selected_cols = c("Subject", "Age", "Sex", "hand"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P54E3", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_29$Gender[tolower(result_29$Gender) == "female"] <- "Female"
result_29$Gender[tolower(result_29$Gender) == "male"] <- "Male"

result_29$Handedness[tolower(result_29$Handedness) == "right"] <- "R"
result_29$Handedness[tolower(result_29$Handedness) == "left"] <- "L"

# Sui_2014

data_30 <- read.csv("./2_Raw_Data/Sui_2014/Sui_2014_Exp1_raw.csv") 

result_30 <- extract_and_standardize(
  df = data_30,
  selected_cols = c("Subject", "Age", "Sex", "hand"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps3E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_30$Gender[tolower(result_30$Gender) == "female"] <- "Female"
result_30$Gender[tolower(result_30$Gender) == "male"] <- "Male"

# Sui_2014_APP
# Exp1

data_31 <- read.csv("./2_Raw_Data/Sui_2014_APP/Sui_2014_APP_Exp1_raw.csv") 

result_31 <- extract_and_standardize(
  df = data_31,
  selected_cols = c("subj_idx", "Age", "Sex", "hand"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps5E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Exp2

data_32 <- read.csv("./2_Raw_Data/Sui_2014_APP/Sui_2014_APP_Exp2_raw.csv") 

result_32 <- extract_and_standardize(
  df = data_32,
  selected_cols = c("subj_idx", "Age", "Sex", "hand"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps5E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Exp3

data_33 <- read.csv("./2_Raw_Data/Sui_2014_APP/Sui_2014_APP_Exp3_raw.csv") 

result_33 <- extract_and_standardize(
  df = data_33,
  selected_cols = c("subj_idx", "Age", "Sex", "hand"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps5E3", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Exp4

data_34 <- read.csv("./2_Raw_Data/Sui_2014_APP/Sui_2014_APP_Exp4_raw.csv") 

result_34 <- extract_and_standardize(
  df = data_34,
  selected_cols = c("subj_idx", "Age", "Sex", "hand"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Ps5E4", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Sui_2015 

# 暂时略过

# Sui_2023_CC

data_36 <- read.csv("./2_Raw_Data/Sui_2023_CC/Sui_2023_CC_Exp1_raw.csv") 

result_36 <- extract_and_standardize(
  df = data_36,
  selected_cols = c("subj_idx", "Age", "Sex", "hand"),
  rename_map = list("subj_idx" = "Subject_ID",
                    "Sex" = "Gender",
                    "hand" = "Handedness"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt18E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Vicovaro_2022_EPHPP
# Exp1

data_37 <- read.csv("./2_Raw_Data/Vicovaro_2022_EPHPP/Vicovaro_2022_EPHPP_Exp1_raw.csv") 

result_37 <- extract_and_standardize(
  df = data_37,
  selected_cols = c("participant_id", "age", "sex", "handedness"),
  rename_map = list("participant_id" = "Subject_ID",
                    "sex" = "Gender",
                    "handedness" = "Handedness",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt6E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_37$Gender[tolower(result_37$Gender) == "f"] <- "Female"
result_37$Gender[tolower(result_37$Gender) == "m"] <- "Male"

result_37$Handedness[tolower(result_37$Handedness) == "right"] <- "R"
result_37$Handedness[tolower(result_37$Handedness) == "left"] <- "L"

# Exp2

data_38 <- read.csv("./2_Raw_Data/Vicovaro_2022_EPHPP/Vicovaro_2022_EPHPP_Exp2_raw.csv") 

result_38 <- extract_and_standardize(
  df = data_38,
  selected_cols = c("participant_id", "age", "sex", "handedness"),
  rename_map = list("participant_id" = "Subject_ID",
                    "sex" = "Gender",
                    "handedness" = "Handedness",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pt6E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_38$Gender[tolower(result_38$Gender) == "f"] <- "Female"
result_38$Gender[tolower(result_38$Gender) == "m"] <- "Male"

result_38$Handedness[tolower(result_38$Handedness) == "right"] <- "R"
result_38$Handedness[tolower(result_38$Handedness) == "left"] <- "L"

# 将Handedness列中的"rightx"替换为"R"
result_38$Handedness[result_38$Handedness == "rightx"] <- "R"

# 替换空白行：
result_38[result_38 == ""] <- "/"

# Wozniak_2018_PLOS
# Exp1

data_39 <- read.csv("./2_Raw_Data/Wozniak_2018_PLOS/Wozniak_2018_PLOS_Exp1_raw.csv") 

result_39 <- extract_and_standardize(
  df = data_39,
  selected_cols = c("Subject", "age", "sex", "handedness"),
  rename_map = list("Subject" = "Subject_ID",
                    "sex" = "Gender",
                    "handedness" = "Handedness",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P95E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Exp2

data_40 <- read.csv("./2_Raw_Data/Wozniak_2018_PLOS/Wozniak_2018_PLOS_Exp2_raw.csv") 

result_40 <- extract_and_standardize(
  df = data_40,
  selected_cols = c("Subject", "age", "sex", "handedness"),
  rename_map = list("Subject" = "Subject_ID",
                    "sex" = "Gender",
                    "handedness" = "Handedness",
                    "age" = "Age"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "P95E2", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Wozniak_2022_PR
# Exp1

data_41 <- read.csv("./2_Raw_Data/Wozniak_2022_PR/Wozniak_2022_PR_Exp1_raw.csv") 

result_41 <- extract_and_standardize(
  df = data_41,
  selected_cols = c("V1", "V2", "sex", "handedness"),
  rename_map = list("V1" = "Subject_ID",
                    "V2" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

#  Exp2

data_42 <- read.csv("./2_Raw_Data/Wozniak_2022_PR/Wozniak_2022_PR_Exp2_raw.csv") 

result_42 <- extract_and_standardize(
  df = data_42,
  selected_cols = c("V1", "V2", "sex", "handedness"),
  rename_map = list("V1" = "Subject_ID",
                    "V2" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Exp3

data_43 <- read.csv("./2_Raw_Data/Wozniak_2022_PR/Wozniak_2022_PR_Exp3_raw.csv") 

result_43 <- extract_and_standardize(
  df = data_43,
  selected_cols = c("V1", "V2", "sex", "handedness"),
  rename_map = list("V1" = "Subject_ID",
                    "V2" = "Gender"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

# Xu_2021_CP

data_44 <- read.csv("./2_Raw_Data/Xu_2021_CP/Xu_2021_CP_Exp1_raw.csv") 

result_44 <- extract_and_standardize(
  df = data_44,
  selected_cols = c("Subject", "Group", "Sex", "Yes"),
  rename_map = list("Subject" = "Subject_ID",
                    "Sex" = "Gender",
                    "Group" = "group",
                    "Yes" = "Yes"),
  standard_order = DEFAULT_STANDARD_COLUMNS,
  exp_id_value = "Pn23E1", 
  deduplicate_by_subject = TRUE,
  verbose = TRUE
)

result_44$Gender[tolower(result_44$Gender) == "female"] <- "Female"
result_44$Gender[tolower(result_44$Gender) == "male"] <- "Male"

 