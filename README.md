# ZWFTOOLS工具包详解

GITHUB 项目地址[OutKeal/abap_zwftools: ABAP我的工具包 (github.com)](https://github.com/OutKeal/abap_zwftools)

该项目包含一些个人常用的ABAP公共类/程序.

部分程序来自于其它人分享.部分原创.

## 工具类

### ZWFT_COMMON 常用方法

集成了大量静态方法,实现常用功能.大概包括以下功能:

-   物料/供应商/客户的模糊搜索.
    -   SEARCH_VENDOR
    -   SEARCH_CUSTOMER
    -   SEARCH_MATERIAL
-   弹框选择数据/确认提交/确认事件
    -   SEARCH_DATA
    -   CONFIRM
    -   CONFIRM_DATE
-   获取GUID
    -   GET_GUID32
    -   GET_GUID16
-   获取调用栈(SYSTEM_CALLSTACK)
-   添加GOS连接(ADD_GOS_RELATIONSHIP)
-   数据检查,转换
    -   NUMBER_CHECK
    -   NUMBER_INPUT,
    -   NUMBER_TO_CHINESE,
    -   NUMBER_CONV_END_NO_ZERO,
    -   DATE_OUTPUT,
    -   DATE_INPUT,
    -   DATE_TO_CHINESE
-   通用调用事务
    -   CALL_TRANSATION
    -   CALL_TRANSATION_BY_LINE
-   文件下载/上传
    -   FILE_DOWNLOAD_TO_CSV,
    -   FILE_DOWNLOAD_TO_EXCEL,
    -   FILE_UPLOAD_BIN,
    -   FILE_UPLOAD_FROM_EXCEL,
    -   FILE_GET_READ_PATH,
    -   FILE_GET_SAVE_PATH,
    -   FILE_DOWNLOAD_TEMPLATE
-   进度条 PROGRESSBAR_SHOW
-   数据元素获取值,值校验
    -   DOMA_VALUE_CHECK
    -   DOMA_VALUE_GET_MULTIPLE
    -   DOMA_VALUE_GET_SINGLE

-   ALV FCAT处理
    -   FCAT_FROM_DATA
    -   FCAT_FROM_NAME
    -   FCAT_FROM_CONFIG
    -   FCAT_SET_VALUE
-   表解析
    -   GET_FIELDS
    -   GET_FIELDS_DFIES_BY_DATA
    -   GET_FIELDS_DFIES
    -   CREATE_TABLE_DFIES
    -   CREATE_TABLE_COMPO
    -   CREATE_TABLE_FCAT
    -   GET_COMP_FROM_DATA
-   获取用户参数
    -   GET_USER_PARAMETER
-   获取屏幕字段,GUI状态字段
    -   GET_DYNNR_FIELD
    -   GET_STATUS_FUNCTIONS
-   屏幕分辨率
    -   GET_SCREEN_XY_POINT
-   获取函数参数
    -   GET_FUNCTION_INTERFACE
-   注册表操作
    -   REG_GET_DWORD
    -   REG_SET_DWORD
    -   REG_SET_WORD
    -   REG_GET_WORD


## 其它类

-   ZWFT_AUTH_CHECK 常用组织架构的单值/批量权限检查.

-   ZWFT_DYNAMIC_SCREEN 动态生成一个屏幕

-   ZWFT_HTML HTML网页预览/打印

配合HTML/CSS/JS模板使用.

- ZWFT_INVOICE_ORC 实现发票图片识别

- ZWFT_INV_CLASS 配合CDS,程序,实现一个高性能进销存报表

​		[CDS 进销存报表功能实现-CSDN博客](https://blog.csdn.net/qq_33852362/article/details/136624782)

- ZWFT_MESSAGE 消息收集器

- ZWFT_REGEX_GET 常用正则

- ZWFT_SINGLE_READ 常用SINGLE READ

例如取供应商名称,代码简洁:

```abap
lifnr_name = zwft_single_read=>lfa1( <item>-lifnr )-name1.
```

## 收集的程序

- ZWFT_ACTION_USER 用户操作明细的报表

- ZWFT_AL11 一个开源的AL11增强版本

- ZWFT_BC001/ZWFT_BC002/ZWFT_BC003,用户/角色/分配/派生的批导入程序

- ZWFT_BDC 快捷BDC操作

- ZWFT_INV/ZWFT_INV_MSEG/ZWFT_INV_DIFF 进销存实现

- ZWFT_JOB_LIST 作业清单报表

- ZWFT_MENU 一个简单的HTML按钮菜单

配合HTML/CSS使用

- ZWFT_MENU_SET 遍历标准菜单,生成MENU菜单的自定义配置
- ZWFT_PROGRAM_SCAN 程序扫描
- ZWFT_REPORT_DOWNLOAD 报表显示结果的下载
- ZWFT_SQL_TOAD 开源的SQL编辑器
- ZWFT_TABLE 表操作功能,集成查看,下载,上传等
- ZWFT_TABLE_DOWNLOAD 表下载
- ZWFT_TABLE_DOWN_MASS 批量表下载
- ZWFT_TABLE_UPLOAD_MASS 批量表上传
- ZWFT_TABLE_INPUT 表大量内容导入
- ZWFT_TABLE_TRAN 跨系统配置表同步
- ZWFT_TRANS 好用的快速翻译工具
- ZWFT_USEREXIT_FIND/ZWFT_BADI_FIND,找增强.

### 魔改版本FALV

ZWFT_FALV,COPY ZCL_FALV开源工程,个人易用化改造版本.









​                                                             
